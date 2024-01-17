from typing import Optional

import aiohttp
import asyncio


def url(port: int, scenario: int, param: Optional[str] = None):
    base_url = f'http://localhost:{port}/{scenario}'
    if param:
        return base_url + f'?{param}'
    return base_url


class FirstCompletedTaskGroup(asyncio.TaskGroup):
    def __init__(self):
        super().__init__()
        self.__tasks = []
        self.winner = None

    def cancel_others(self, task):
        if not task.cancelled():
            self.__tasks.remove(task)
            for t in self.__tasks:
                t.cancel()
            self.winner = task.result()

    def create_task(self, coro, *, name=None, context=None):
        task = super().create_task(coro, name=name, context=context)
        task.add_done_callback(self.cancel_others)
        self.__tasks.append(task)
        return task

    def result(self):
        return self.winner


async def scenario1(port: int):
    async with aiohttp.ClientSession() as session:
        async def req():
            async with session.get(url(port, 1)) as response:
                return await response.text()

        async with FirstCompletedTaskGroup() as group:
            group.create_task(req())
            group.create_task(req())

        return group.result()


async def scenario2(port: int):
    async with aiohttp.ClientSession() as session:
        async def req():
            try:
                async with session.get(url(port, 2)) as response:
                    return await response.text()
            except aiohttp.client_exceptions.ServerDisconnectedError:
                raise asyncio.CancelledError

        async with FirstCompletedTaskGroup() as group:
            group.create_task(req())
            group.create_task(req())

        return group.result()


# note: requires increasing max number of open files `ulimit -n 16000`
async def scenario3(port: int):
    connector = aiohttp.TCPConnector(limit=10_000)
    async with aiohttp.ClientSession(connector=connector) as session:
        async def req():
            async with session.get(url(port, 3)) as response:
                return await response.text()

        async with FirstCompletedTaskGroup() as group:
            [group.create_task(req()) for _ in range(10_000)]

        return group.result()


async def scenario4(port: int):
    async with aiohttp.ClientSession() as session:
        async def req():
            async with session.get(url(port, 4)) as response:
                return await response.text()

        async def req_with_timeout():
            try:
                async with asyncio.timeout(1):
                    return await req()
            except TimeoutError:
                raise asyncio.CancelledError

        async with FirstCompletedTaskGroup() as group:
            group.create_task(req())
            group.create_task(req_with_timeout())

        return group.result()


async def scenario5(port: int):
    async with aiohttp.ClientSession() as session:
        async def req():
            async with session.get(url(port, 5)) as response:
                if response.status != 200:
                    raise asyncio.CancelledError
                return await response.text()

        async with FirstCompletedTaskGroup() as group:
            group.create_task(req())
            group.create_task(req())

        return group.result()


async def scenario6(port: int):
    async with aiohttp.ClientSession() as session:
        async def req():
            async with session.get(url(port, 6)) as response:
                if response.status != 200:
                    raise asyncio.CancelledError
                return await response.text()

        async with FirstCompletedTaskGroup() as group:
            [group.create_task(req()) for _ in range(3)]

        return group.result()


async def scenario7(port: int):
    async with aiohttp.ClientSession() as session:
        async def req():
            async with session.get(url(port, 7)) as response:
                return await response.text()

        async def hedge():
            await asyncio.sleep(3)
            return await req()

        async with FirstCompletedTaskGroup() as group:
            group.create_task(req())
            group.create_task(hedge())

        return group.result()


async def scenario8(port: int):
    async with aiohttp.ClientSession() as session:
        async def req(param: str):
            response = await session.get(url(port, 8, param))
            if response.status != 200:
                raise asyncio.CancelledError
            return await response.text()

        async def open_req():
            return await req("open")

        async def use_req(my_id: str):
            return await req(f'use={my_id}')

        async def close_req(my_id: str):
            return await req(f'close={my_id}')

        async def closeable_req():
            my_id = await open_req()
            try:
                resp = await use_req(my_id)
            finally:
                await close_req(my_id)
            return resp

        async with FirstCompletedTaskGroup() as group:
            group.create_task(closeable_req())
            group.create_task(closeable_req())

        return group.result()


async def scenario9(port: int):
    async with aiohttp.ClientSession() as session:
        async def req():
            async with session.get(url(port, 9)) as response:
                if response.status != 200:
                    raise asyncio.CancelledError
                from datetime import datetime
                now = datetime.now()
                return now, await response.text()

        async with asyncio.TaskGroup() as group:
            tasks = [group.create_task(req()) for _ in range(10)]

        valid = list(filter(lambda task: not task.cancelled(), tasks))

        time_and_letters = [task.result() for task in valid]

        ordered = sorted(time_and_letters, key=lambda time_and_letter: time_and_letter[0])

        import functools

        return functools.reduce(lambda acc, time_and_letter: acc + time_and_letter[1], ordered, "")


async def main():
    result1 = await scenario1(8080)
    print(result1)

    result2 = await scenario2(8080)
    print(result2)

    result3 = await scenario3(8080)
    print(result3)

    result4 = await scenario4(8080)
    print(result4)

    result5 = await scenario5(8080)
    print(result5)

    result6 = await scenario6(8080)
    print(result6)

    result7 = await scenario7(8080)
    print(result7)

    result8 = await scenario8(8080)
    print(result8)

    result9 = await scenario9(8080)
    print(result9)

if __name__ == "__main__":
    asyncio.run(main())
