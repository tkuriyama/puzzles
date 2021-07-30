import asyncio, time

async def f():
    await asyncio.sleep(1.0)
    return 123

async def main():
    print(f'{time.ctime()}')
    result = await f()
    print(f'{time.ctime()}')
    return result

asyncio.run(main())
