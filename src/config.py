from dataclasses import dataclass

@dataclass
class Config:
    grid_size: tuple[int, int] = (720, 1080)
    update_frequency: float = 30

    # really good one but also works with 0.07 and 0.1 and freq 20
    #sigma: float = 0.08
    #mu: float = 0.11
    
    # another one freq 30
    #sigma: float = 0.08
    #mu: float = 0.11

    sigma: float = 0.07
    mu: float = 0.1