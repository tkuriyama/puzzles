import asyncio

async def f(delay):
    print(delay)
    await asyncio.sleep(1 / delay)
    return delay

loop = asyncio.get_event_loop()
for i in range(10):
    loop.create_task(f(i))

pending = asyncio.all_tasks(loop=loop)
group = asyncio.gather(*pending, return_exceptions=True)
results = loop.run_until_complete(group)
print(f'Results: {results}')
loop.close()
