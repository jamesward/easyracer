import aiohttp
import asyncio


async def req(session: aiohttp.ClientSession, scenario: int):
    async with session.get(f'http://localhost:8080/{scenario}') as response:
        print("Status:", response.status)
        return await response.text()


async def scenario1(session: aiohttp.ClientSession):
    reqs = [asyncio.create_task(req(session, 1)), asyncio.create_task(req(session, 1))]
    done, pending = await asyncio.wait(reqs, return_when=asyncio.FIRST_COMPLETED)
    for task in pending:
        task.cancel()
    return await list(done)[0]

async def scenario2(session: aiohttp.ClientSession):
    reqs = [asyncio.create_task(req(session, 2)), asyncio.create_task(req(session, 2))]
    done, pending = await asyncio.wait(reqs, return_when=asyncio.FIRST_COMPLETED)
    for task in pending:
        task.cancel()
    return await list(done)[0]

# currently not working
async def scenario3(session: aiohttp.ClientSession):
    try:
        async with asyncio.timeout(5):  # temporary timeout to avoid blocking here
            reqs = [asyncio.create_task(req(session, 3)) for _ in range(10_000)]
            iterable = await asyncio.wait(reqs, return_when=asyncio.FIRST_COMPLETED)
            done = next(iterable)
            print(done)
            return await done
    except asyncio.exceptions.TimeoutError:
        return "wrong"

# currently not working
async def scenario4(session: aiohttp.ClientSession):
    task = asyncio.create_task(req(session, 4))
    task_with_timeout = asyncio.wait_for(asyncio.create_task(req(session, 4)), timeout=1)
    reqs = [task, task_with_timeout]
    done, pending = await asyncio.wait(reqs, return_when=asyncio.FIRST_COMPLETED)
    for task in pending:
        task.cancel()
    return await list(done)[0]

# currently not working
async def scenario5(session: aiohttp.ClientSession):
    task = asyncio.create_task(req(session, 5))
    task_with_valid_response = asyncio.create_task(req(session, 5))
    # todo: filter response
    reqs = [task, task_with_valid_response]
    done, pending = await asyncio.wait(reqs, return_when=asyncio.FIRST_COMPLETED)
    print(done, pending)

    for task in pending:
        task.cancel()
    return await list(done)[0]

# currently not working
async def scenario6(session: aiohttp.ClientSession):
    raise NotImplementedError

# currently not working
async def scenario7(session: aiohttp.ClientSession):
    raise NotImplementedError


async def main():
    async with aiohttp.ClientSession() as session:
        result1 = await scenario1(session)
        print(result1)

        result2 = await scenario2(session)
        print(result2)

        result3 = await scenario3(session)
        print(result3)

        result4 = await scenario4(session)
        print(result4)

        result5 = await scenario5(session)
        print(result5)

        result6 = await scenario6(session)
        print(result6)

        result7 = await scenario7(session)
        print(result7)

        # todo: validate the rights

asyncio.run(main())