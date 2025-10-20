from LifeForm import LifeForm
from typing import Dict
import numpy as np

class Grid:
    def __init__(self, grid_size: tuple[int, int]):
        self.grid_size = grid_size
        self.colors: Dict[str, np.array] = {}
        self.lifeforms: Dict[str, LifeForm] = {}

    def add_lifeform(self, lifeform: LifeForm, color_hex: str):
        self.lifeforms[lifeform.name] = lifeform

        # Convert color from hex (or color string) to BGR float [0,1]
        color_rgb = np.array([
            int(color_hex[5:7], 16),
            int(color_hex[3:5], 16),
            int(color_hex[1:3], 16),
        ], dtype=np.float64) / 255.0

        self.colors[lifeform.name] = color_rgb
    
    def random(self, discrete: bool = False):
        for lifeform in self.lifeforms.values():
            lifeform.randomize_state(discrete=discrete)

    def update(self):
        _lifeforms = self.lifeforms.values()
        for lifeform in _lifeforms:
            lifeform.calculate_next_state(_lifeforms)
        
        for lifeform in _lifeforms:
            lifeform.change_state()

    def calculate_displayable_version(self):
        h, w = self.grid_size
        # Start with a black image
        image = np.zeros((h, w, 3), dtype=np.float32)
        
        # Track total intensity at each pixel to calculate mean
        total_intensity = np.zeros((h, w), dtype=np.float32)
        
        for name, lifeform in self.lifeforms.items():
            # Add weighted color contribution
            intensity = lifeform.state.astype(np.float32)
            image += intensity[:, :, None] * self.colors[name][None, None, :]
            total_intensity += intensity
        
        # Calculate mean color by dividing by total intensity
        # Avoid division by zero
        mask = total_intensity > 0
        image[mask] = image[mask] / total_intensity[mask, None]
        
        # Clip to [0,1] and convert to uint8 for OpenCV
        image = np.clip(image, 0, 1)
        return (image * 255).astype(np.uint8)