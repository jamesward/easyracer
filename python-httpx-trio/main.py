from typing import Optional

import httpx
import trio


def url(port: int, scenario: int, param: Optional[str] = None):
    base_url = f'http://localhost:{port}/{scenario}'
    if param:
        return base_url + f'?{param}'
    return base_url


# Note: Request creation code is intentionally not shared across scenarios

async def race(*async_fns):
    if not async_fns:
        raise ValueError("must pass at least one argument")

    winner = None

    async def jockey(async_fn, cancel_scope):
        try:
            nonlocal winner
            print("trying")
            winner = await async_fn()
            print("winner", winner)
            cancel_scope.cancel()
        except BaseException as e:
            print(f"{e=}")
            # pass

    async with trio.open_nursery() as tasks:
        for async_fn in async_fns:
            tasks.start_soon(jockey, async_fn, tasks.cancel_scope)

    return winner


async def scenario1(port: int):
    async with httpx.AsyncClient() as client:
        async def req():
            response = await client.get(url(port, 1))
            return response.text

        return await race(req, req)


async def scenario2(port: int):
    async with httpx.AsyncClient() as client:
        async def req():
            response = await client.get(url(port, 2))
            return response.text

        return await race(req, req)


# currently not working due to some limit of 1016 concurrent connections
async def scenario3(port: int):
    limits = httpx.Limits(max_connections=None)
    async with httpx.AsyncClient(limits=limits) as client:
        async def req():
            response = await client.get(url(port, 3))
            return response.text

        return await race(*[req for _ in range(10_000)])


# currently not working
# timeout is killing the race
async def scenario4(port: int):
    async with httpx.AsyncClient() as client:
        async def req():
            print("req")
            response = await client.get(url(port, 4))
            print(f"{response=}")
            return response.text

        async def req_with_timeout():
            with trio.fail_after(1):
                return await req()

        return await race(req_with_timeout, req)


async def scenario5(port: int):
    async with httpx.AsyncClient() as client:
        async def req():
            response = await client.get(url(port, 5))
            if response.status_code != 200:
                raise Exception("invalid response")
            return response.text

        return await race(req, req)


async def scenario6(port: int):
    async with httpx.AsyncClient() as client:
        async def req():
            response = await client.get(url(port, 6))
            if response.status_code != 200:
                raise Exception("invalid response")
            return response.text

        return await race(req, req, req)


async def scenario7(port: int):
    async with httpx.AsyncClient() as client:
        async def req():
            response = await client.get(url(port, 7))
            return response.text

        async def hedge_req():
            await trio.sleep(3)
            return await req()

        return await race(req, hedge_req)


async def scenario8(port: int):
    async with httpx.AsyncClient() as client:
        async def req(param: str):
            response = await client.get(url(port, 8, param))
            if response.status_code != 200:
                raise Exception("invalid response")
            return response.text

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

        return await race(closeable_req, closeable_req)


async def scenario9(port: int):
    async with httpx.AsyncClient() as client:
        send_channel, receive_channel = trio.open_memory_channel(10)

        async def req():
            response = await client.get(url(port, 9))
            if response.status_code == 200:
                await send_channel.send(response.text)

        async with send_channel:
            async with trio.open_nursery() as nursery:
                [nursery.start_soon(req) for _ in range(10)]

        async with receive_channel:
            return "".join([letter async for letter in receive_channel])


async def main():
    # result1 = await scenario1(8080)
    # print(result1)
    #
    # result2 = await scenario2(8080)
    # print(result2)

    # result3 = await scenario3(8080)
    # print(result3)

    # result4 = await scenario4(8080)
    # print(result4)

    # result5 = await scenario5(8080)
    # print(result5)

    # result6 = await scenario6(8080)
    # print(result6)

    # result7 = await scenario7(8080)
    # print(result7)
    #
    # result8 = await scenario8(8080)
    # print(result8)
    #
    result9 = await scenario9(8080)
    print(result9)


if __name__ == "__main__":
    trio.run(main)
