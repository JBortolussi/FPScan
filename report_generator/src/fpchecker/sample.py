import random

class Sample:
    def __init__(self):
        pass

    def sample(self, N:int, ranges: list[tuple[float, float]]) -> list[list[float]]:
        d = len(ranges)
        n = int(N ** (1 / d))

        steps = [(b[1] - b[0])/n for b in ranges]

        def explore(i: int) -> list[list[float]]:
            if i < d-1:
                next_points = explore(i+1)
                res = []
                for k in range(n+1):
                    res += [pts + [ranges[i][0] + steps[i] * k] for pts in next_points]
                return res
            elif i == d-1:
                return [[ranges[i][0] + k * steps[i]] for k in range(n+1)]
            else:
                raise Exception(f"Impossible: {i}")
        
        return [
            list(reversed(l)) for l in explore(0)
        ]
    
class RandomSample (Sample):
    def sample(self, N: int, ranges: list[tuple[float, float]]) -> list[list[float]]:
        def get_rand_point() -> list[float]:
            p = []
            for l, u in ranges:
                p.append(random.random() * (u - l) + l)

            return p
        return [
            get_rand_point() for _ in range(N)
        ]