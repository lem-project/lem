"""Module docstring.

Copyright (c) 2026 Yuri Flores Albuquerque.
SPDX-License-Identifier: FSL-1.1-MIT
Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
Competing commercial use is strictly prohibited.
"""
import asyncio
from dataclasses import dataclass


@dataclass
class Point:
    """A point."""
    x: float
    y: float = 0.0

    def norm(self) -> float:
        return (self.x ** 2 + self.y ** 2) ** 0.5


async def fetch(url: str, retries: int = 3) -> dict[str, int]:
    for attempt in range(retries):
        try:
            data = await asyncio.sleep(0, result={"url": url})
        except TimeoutError:
            continue
        else:
            return data
        finally:
            print(f"attempt {attempt}: {f'{url!r}'}")
    return {}


def describe(value):
    match value:
        case Point(x=0, y=0):
            return "origin"
        case [first, *rest]:
            return f"list starting with {first}"
        case _:
            return "other"


result = describe(
    Point(1.0, 2.0),
)
