import asyncio
import random


async def download_file(file: str) -> bool:
    status = random.random() < 0.1
    print(f"downloading {file} {'successful' if status else 'failed'}")
    return status


async def download_file_with_retry(file: str):
    while True:
        if await download_file(file):
            return
        await asyncio.sleep(0.01)


async def main(files):
    coros = [download_file_with_retry(file) for file in files]
    await asyncio.gather(*coros)


if __name__ == "__main__":
    import time

    s = time.perf_counter()
    asyncio.run(main(['a', 'b', 'c']))
    elapsed = time.perf_counter() - s
    print(f"executed in {elapsed:0.2f} second(s)")
