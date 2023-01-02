import aiohttp
import asyncio


def url(port: int, scenario: int):
    return f'http://localhost:{port}/{scenario}'


# request creation code is intentionally not shared across scenarios
async def scenario1(session: aiohttp.ClientSession, port: int):
    async def req():
        async with session.get(url(port, 1)) as response:
            return await response.text()

    req1 = asyncio.create_task(req())
    req2 = asyncio.create_task(req())
    reqs = [req1, req2]
    done, pending = await asyncio.wait(reqs, return_when=asyncio.FIRST_COMPLETED)
    for task in pending:
        task.cancel()
    return await list(done)[0]

# not working
async def scenario2(session: aiohttp.ClientSession, port: int):
    async def req():
        async with session.get(url(port, 2)) as response:
            return await response.text()

    req1 = asyncio.create_task(req())
    req2 = asyncio.create_task(req())
    reqs = [req1, req2]
    done, pending = await asyncio.wait(reqs, return_when=asyncio.FIRST_COMPLETED)
    for task in pending:
        task.cancel()
    return await list(done)[0]


# currently not working
async def scenario3(session: aiohttp.ClientSession, port: int):
    async def req():
        async with session.get(url(port, 3)) as response:
            return await response.text()

    try:
        async with asyncio.timeout(5):  # temporary timeout to avoid blocking here
            reqs = [asyncio.create_task(req()) for _ in range(10_000)]
            iterable = await asyncio.wait(reqs, return_when=asyncio.FIRST_COMPLETED)
            done = next(iterable)
            print(done)
            return await done
    except asyncio.exceptions.TimeoutError:
        return "wrong"


# currently not working
async def scenario4(session: aiohttp.ClientSession, port: int):
    async def req():
        async with session.get(url(port, 4)) as response:
            return await response.text()

    req1 = asyncio.create_task(req())
    req2 = asyncio.wait_for(asyncio.create_task(req()), timeout=1)
    reqs = [req1, req2]

    done, pending = await asyncio.wait(reqs, return_when=asyncio.FIRST_COMPLETED)
    for task in pending:
        task.cancel()
    return await list(done)[0]


# note: this does not cancel any other losers
async def scenario5(session: aiohttp.ClientSession, port: int):
    async def req():
        async with session.get(url(port, 5)) as response:
            if response.status != 200:
                raise Exception("invalid response")
            return await response.text()

    req1 = asyncio.create_task(req())
    req2 = asyncio.create_task(req())
    results = await asyncio.gather(req1, req2, return_exceptions=True)

    for result in results:
        if not isinstance(result, Exception):
            return result


async def scenario6(session: aiohttp.ClientSession, port: int):
    async def req():
        async with session.get(url(port, 6)) as response:
            if response.status != 200:
                raise ValueError("invalid response")
            return await response.text()

    req1 = asyncio.create_task(req())
    req2 = asyncio.create_task(req())
    req3 = asyncio.create_task(req())
    reqs = [req1, req2, req3]

    for coro in asyncio.as_completed(reqs):
        try:
            result = await coro

            for req in reqs:
                req.cancel()

            return result
        except ValueError:
            pass


async def scenario7(session: aiohttp.ClientSession, port: int):
    async def req():
        async with session.get(url(port, 7)) as response:
            return await response.text()

    async def hedge():
        await asyncio.sleep(3)
        return await req()

    req1 = asyncio.create_task(req())
    req2 = asyncio.create_task(hedge())
    reqs = [req1, req2]
    done, pending = await asyncio.wait(reqs, return_when=asyncio.FIRST_COMPLETED)
    for task in pending:
        task.cancel()
    return await list(done)[0]


# currently not working
async def scenario8(session: aiohttp.ClientSession, port: int):
    raise NotImplementedError


# currently not working
async def scenario9(session: aiohttp.ClientSession, port: int):
    async def req():
        resp = await session.get(url(port, 9))
        from datetime import datetime
        now = datetime.now()
        return now, resp

    reqs = [asyncio.create_task(req()) for _ in range(10)]
    (done, _) = await asyncio.wait(reqs, return_when=asyncio.ALL_COMPLETED)
    valid = list(filter(lambda task: task.result()[1].status == 200, done))

    async def text(task):
        return task.result()[0], await task.result()[1].text()

    letters_async = list(map(text, valid))

    print(letters_async)

    letters = await asyncio.gather(letters_async)

    print(letters)

    ordered = sorted(letters, key=lambda time, _: time)

    print(ordered)

    import functools

    return functools.reduce(lambda acc, letter: acc + letter, ordered, "")


async def main():
    async with aiohttp.ClientSession() as session:
        # result1 = await scenario1(session, 8080)
        # print(result1)
        #
        # result2 = await scenario2(session, 8080)
        # print(result2)
        #
        # result3 = await scenario3(session, 8080)
        # print(result3)
        #
        # result4 = await scenario4(session, 8080)
        # print(result4)
        #
        # result5 = await scenario5(session, 8080)
        # print(result5)

        result9 = await scenario9(session, 8080)
        print(result9)

        # result7 = await scenario7(session, 8080)
        # print(result7)

if __name__ == "__main__":
    asyncio.run(main())
