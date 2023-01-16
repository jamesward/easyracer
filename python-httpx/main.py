import httpx
import asyncio


def url(port: int, scenario: int):
    return f'http://localhost:{port}/{scenario}'


# Note: Request creation code is intentionally not shared across scenarios


async def scenario1(port: int):
    async with httpx.AsyncClient() as client:
        async def req():
            response = await client.get(url(port, 1))
            return response.text

        req1 = asyncio.create_task(req())
        req2 = asyncio.create_task(req())
        reqs = [req1, req2]
        done, pending = await asyncio.wait(reqs, return_when=asyncio.FIRST_COMPLETED)
        for task in pending:
            task.cancel()
        return await list(done)[0]


async def scenario2(port: int):
    async with httpx.AsyncClient() as client:
        async def req():
            response = await client.get(url(port, 2))
            return response.text

        req1 = asyncio.create_task(req())
        req2 = asyncio.create_task(req())
        reqs = [req1, req2]
        for coro in asyncio.as_completed(reqs):
            try:
                result = await coro

                for req in reqs:
                    req.cancel()

                return result
            except httpx.RemoteProtocolError:
                pass


# currently not working
async def scenario3(port: int):
    limits = httpx.Limits(max_connections=None)
    async with httpx.AsyncClient(limits=limits) as client:
        async def req():
            response = await client.get(url(port, 3))
            return response.text

        reqs = [asyncio.create_task(req()) for _ in range(10_000)]
        iterable = await asyncio.wait(reqs, return_when=asyncio.FIRST_COMPLETED)
        done = next(iterable)
        print(done)
        return await done


# # currently not working
# async def scenario4(port: int):
#     async with aiohttp.ClientSession() as session:
#         async def req():
#             async with session.get(url(port, 4)) as response:
#                 return await response.text()
#
#         req1 = asyncio.create_task(req())
#         req2 = asyncio.wait_for(asyncio.create_task(req()), timeout=1)
#         reqs = [req1, req2]
#
#         done, pending = await asyncio.wait(reqs, return_when=asyncio.FIRST_COMPLETED)
#         for task in pending:
#             task.cancel()
#         return await list(done)[0]
#
#
# # note: this does not cancel any other losers
# async def scenario5(port: int):
#     async with aiohttp.ClientSession() as session:
#         async def req():
#             async with session.get(url(port, 5)) as response:
#                 if response.status != 200:
#                     raise Exception("invalid response")
#                 return await response.text()
#
#         req1 = asyncio.create_task(req())
#         req2 = asyncio.create_task(req())
#         results = await asyncio.gather(req1, req2, return_exceptions=True)
#
#         for result in results:
#             if not isinstance(result, Exception):
#                 return result
#
#
# async def scenario6(port: int):
#     async with aiohttp.ClientSession() as session:
#         async def req():
#             async with session.get(url(port, 6)) as response:
#                 if response.status != 200:
#                     raise ValueError("invalid response")
#                 return await response.text()
#
#         req1 = asyncio.create_task(req())
#         req2 = asyncio.create_task(req())
#         req3 = asyncio.create_task(req())
#         reqs = [req1, req2, req3]
#
#         for coro in asyncio.as_completed(reqs):
#             try:
#                 result = await coro
#
#                 for req in reqs:
#                     req.cancel()
#
#                 return result
#             except ValueError:
#                 pass
#
#
# async def scenario7(port: int):
#     async with aiohttp.ClientSession() as session:
#         async def req():
#             async with session.get(url(port, 7)) as response:
#                 return await response.text()
#
#         async def hedge():
#             await asyncio.sleep(3)
#             return await req()
#
#         req1 = asyncio.create_task(req())
#         req2 = asyncio.create_task(hedge())
#         reqs = [req1, req2]
#         done, pending = await asyncio.wait(reqs, return_when=asyncio.FIRST_COMPLETED)
#         for task in pending:
#             task.cancel()
#         return await list(done)[0]
#
#
# # currently not working
# async def scenario8(port: int):
#     async with aiohttp.ClientSession() as session:
#         raise NotImplementedError
#
#
# async def scenario9(port: int):
#     async with aiohttp.ClientSession() as session:
#         async def req():
#             resp = await session.get(url(port, 9))
#             from datetime import datetime
#             now = datetime.now()
#             return now, resp
#
#         reqs = [asyncio.create_task(req()) for _ in range(10)]
#         (done, _) = await asyncio.wait(reqs, return_when=asyncio.ALL_COMPLETED)
#         valid = list(filter(lambda task: task.result()[1].status == 200, done))
#
#         async def text(task):
#             return task.result()[0], await task.result()[1].text()
#
#         letters = [await text(task) for task in valid]
#
#         ordered = sorted(letters, key=lambda time_and_letter: time_and_letter[0])
#
#         import functools
#
#         return functools.reduce(lambda acc, time_and_letter: acc + time_and_letter[1], ordered, "")


async def main():
    result1 = await scenario1(8080)
    print(result1)

    result2 = await scenario2(8080)
    print(result2)

    result3 = await scenario3(8080)
    print(result3)

    # result4 = await scenario4(8080)
    # print(result4)
    #
    # result5 = await scenario5(8080)
    # print(result5)
    #
    # result6 = await scenario6(8080)
    # print(result6)
    #
    # result7 = await scenario7(8080)
    # print(result7)
    #
    # result8 = await scenario8(8080)
    # print(result8)
    #
    # result9 = await scenario9(8080)
    # print(result9)


if __name__ == "__main__":
    asyncio.run(main())
