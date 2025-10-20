import cv2
from Grid import Grid
from Rule import Rule
from kernel import Ring, Kernel, KernelVisualizer
from LifeForm import LifeForm
import numpy as np

GRID_SIZE = (720, 1080)

def run(grid: Grid):
    while True:
        # Updates current state
        grid.update()

        # Display the window
        frame = grid.calculate_displayable_version()
        cv2.imshow('Continuous Automata', frame)

        # Wait for any key press and stops if the key pressed is 'q'
        if cv2.waitKey(1) & 0xFF == ord('q'):
            break

    # Clean up
    cv2.destroyAllWindows()



def create_walking_blob(name: str) -> LifeForm:
    kernel = Ring(
        outer_diameter=21, 
        inner_diameter=15,     # 41 * 0.37
        gaussian_sigma=1.2,
        gaussian_kernel_size=15
    ) - Ring(
        outer_diameter=41, 
        inner_diameter=31,     # 41 * 0.37
        gaussian_sigma=1.1,
        gaussian_kernel_size=13
    ) / 2

    kernel.normalize()

    def func(x):
        diff = (x - 0.11) / 0.08
        return 2 * np.exp(-(diff**2) / 2) - 1

    rule = Rule(0.05, kernel, func)

    form = LifeForm(name, GRID_SIZE)
    form.add_rule(name, rule)
    return form

def create_walking_blob2(name: str) -> LifeForm:
    kernel = Ring(
        outer_diameter=21, 
        inner_diameter=15,     # 41 * 0.37
        gaussian_sigma=1.2,
        gaussian_kernel_size=15
    ) - Ring(
        outer_diameter=41, 
        inner_diameter=31,     # 41 * 0.37
        gaussian_sigma=1.1,
        gaussian_kernel_size=13
    ) / 2

    kernel.normalize()

    def func(x):
        diff = (x - 0.11) / 0.08
        return 2 * np.exp(-(diff**2) / 2) - 1

    rule = Rule(0.05, kernel, func)

    form = LifeForm(name, GRID_SIZE)
    form.add_rule(name, rule)
    return form


if __name__ == "__main__":
    underpopulation = 2
    overpopulation = 3
    reproduction = 3

    def growth_func(x: np.ndarray):
        grid = np.zeros(GRID_SIZE)
        grid = grid - (x < underpopulation).astype(int)
        grid = grid - (x > overpopulation).astype(int)
        grid = grid + (x == reproduction).astype(int)
        print(grid)

        return grid.astype(np.float64)
    
    conway_form = LifeForm("conways", GRID_SIZE)

    arr = np.array([
        [1, 1, 1],
        [1, 0, 1],
        [1, 1, 1]
    ], dtype=np.float64)

    kernel = Kernel(arr)
    rule = Rule(1, kernel, growth_func)
    conway_form.add_rule(conway_form.name, rule, 1)

    kv = KernelVisualizer()
    kv.add_kernel("conway", kernel)
    kv.visualize_kernels()

    grid = Grid(GRID_SIZE)

    grid.add_lifeform(conway_form, "#FFFFFF")
    #grid.add_lifeform(form3, "#0000FF")
    grid.random(discrete=True)

    run(grid)