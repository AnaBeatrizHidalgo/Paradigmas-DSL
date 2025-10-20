import numpy as np
from kernel import Kernel
from typing import Callable

class Rule:
    def __init__(self, dt: float, kernel: Kernel, func: Callable[[np.ndarray], np.ndarray]):
        self.dt = dt
        self.kernel = kernel
        self.func = func

    def apply(self, frame: np.ndarray, *, to: np.ndarray) -> np.ndarray:
        growth = self.func(self.kernel(frame))
        return np.clip(to + growth * self.dt, 0, 1)