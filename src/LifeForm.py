from __future__ import annotations
from Rule import Rule
import numpy as np
from typing import Dict

class LifeForm:
    def __init__(self, name: str, grid_size: tuple[int, int]):
        self.name = name
        self._grid_size = grid_size
        self.state = np.zeros(grid_size, dtype=np.float64)
        self._next_state = np.zeros(grid_size, dtype=np.float64)

        self._rules: Dict[str, Rule] = {}    # mapping lifeform name → Rule
        self._weights: Dict[str, float] = {} # mapping lifeform name → weight

    def randomize_state(self, discrete: bool = False):
        self.state = np.random.random(self._grid_size)
        self.state = self.state.astype(dtype=np.float64)
        if discrete:
            self.state = (self.state > 0.5).astype(int).astype(np.float64)

    def add_rule(self, lifeform_name: str, rule: Rule, weight: float=1.0):
        self._weights[lifeform_name] = weight
        self._rules[lifeform_name] = rule

    def calculate_next_state(self, lifeforms: list[LifeForm]):
        self._next_state = np.zeros(self._grid_size)
        for lifeform in lifeforms:
            if lifeform.name not in self._rules:
                continue
            
            weight = self._weights[lifeform.name]
            result = self._rules[lifeform.name].apply(lifeform.state, to=self.state)
            self._next_state += weight * result

    def change_state(self):
        self.state = self._next_state
