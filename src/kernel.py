import cv2
import numbers
import numpy as np
from math import ceil
import matplotlib.pyplot as plt

class Kernel:
    def __init__(self, grid: np.ndarray):
        self.grid = grid

    @staticmethod
    def __correctly_align_grids(grid_a: np.ndarray, grid_b: np.ndarray) -> tuple[np.ndarray, np.ndarray]:
        # returns aligned grids in order of entry
        if grid_a.shape[0] >= grid_b.shape[0] and grid_a.shape[1] >= grid_b.shape[1]:
            corrected_small = np.zeros(grid_a.shape, dtype=np.float32)
            
            # Compute starting indices
            start_x = (grid_a.shape[0] - grid_b.shape[0]) // 2
            start_y = (grid_a.shape[1] - grid_b.shape[1]) // 2

            corrected_small[start_x:start_x+grid_b.shape[0], start_y:start_y+grid_b.shape[1]] = grid_b
            
            return  grid_a, corrected_small
        
        elif grid_b.shape[0] >= grid_a.shape[0] and grid_b.shape[1] >= grid_a.shape[1]:
            corrected_small = np.zeros(grid_b.shape, dtype=np.float64)
            
            # Compute starting indices
            start_x = (grid_b.shape[0] - grid_a.shape[0]) // 2
            start_y = (grid_b.shape[1] - grid_a.shape[1]) // 2

            corrected_small[start_x:start_x+grid_a.shape[0], start_y:start_y+grid_a.shape[1]] = grid_a
            
            return corrected_small, grid_b
        
        else:
            raise ValueError("One kernel grid must be bigger in both dimensions")

    def normalize(self):
        positive_sum = np.sum(self.grid[self.grid > 0])
        if positive_sum > 0:
            self.grid /= positive_sum

    def __add__(self, other):
        if not isinstance(other, Kernel):
            return NotImplemented
        
        aligned_self, aligned_other = Kernel.__correctly_align_grids(self.grid, other.grid)

        return Kernel(aligned_self + aligned_other)
    
    def __sub__(self, other):
        if not isinstance(other, Kernel):
            return NotImplemented
        
        aligned_self, aligned_other = Kernel.__correctly_align_grids(self.grid, other.grid)

        return Kernel(aligned_self - aligned_other)
    
    def __truediv__(self, other):
        if not isinstance(other, numbers.Number):
            raise TypeError(f"Unsupported type for division: {type(other).__name__}")

        return Kernel(self.grid / other)

    def __mul__(self, other):
        if not isinstance(other, (int, float)):
            raise TypeError(f"Unsupported type for multiplication: {type(other).__name__}")
        
        return Kernel(self.grid * other)

    def __neg__(self):
        return self.__mul__(-1)

    def __rmul__(self, other):  # for cases like 3 * Number(5)
        return self.__mul__(other)

    def __call__(self, frame: np.ndarray):
        return cv2.filter2D(frame, -1, self.grid)
        

class Disk(Kernel):
    def __init__(self, radius, *, gaussian_sigma=0.5, gaussian_kernel_size=5):
        kernel = cv2.getStructuringElement(cv2.MORPH_ELLIPSE, (radius*2, radius*2))
        kernel = kernel.astype(np.float64)

        if gaussian_sigma != 0:
            kernel_radius = (gaussian_kernel_size - 1) / 2
            truncate = kernel_radius / gaussian_sigma
            padding_width = ceil(truncate * gaussian_sigma)

            kernel = np.pad(kernel, pad_width=padding_width, mode='constant', constant_values=0)
        
        super().__init__(cv2.GaussianBlur(kernel, (gaussian_kernel_size, gaussian_kernel_size), gaussian_sigma))


class Ring(Kernel):
    def __init__(self, outer_diameter, inner_diameter, *, gaussian_sigma=0.5, gaussian_kernel_size=5):
        if inner_diameter >= outer_diameter:
            raise ValueError("Inner radius must be smaller than outer radius")
        
        # Create outer disk
        outer_kernel = cv2.getStructuringElement(cv2.MORPH_ELLIPSE, (outer_diameter, outer_diameter))
        outer_kernel = outer_kernel.astype(np.float64)
        
        # Create inner disk
        inner_kernel = cv2.getStructuringElement(cv2.MORPH_ELLIPSE, (inner_diameter, inner_diameter))
        inner_kernel = inner_kernel.astype(np.float64)
        
        # Center the inner kernel within the outer kernel dimensions
        ring_kernel = np.zeros_like(outer_kernel, dtype=np.float64)
        start_x = (outer_kernel.shape[0] - inner_kernel.shape[0]) // 2
        start_y = (outer_kernel.shape[1] - inner_kernel.shape[1]) // 2
        ring_kernel[start_x:start_x+inner_kernel.shape[0], start_y:start_y+inner_kernel.shape[1]] = inner_kernel
        
        # Subtract inner from outer to create ring
        kernel = outer_kernel - ring_kernel
        
        if gaussian_sigma != 0:
            # Apply Gaussian blur padding and blurring
            kernel_radius = (gaussian_kernel_size - 1) / 2
            truncate = kernel_radius / gaussian_sigma
            padding_width = ceil(truncate * gaussian_sigma)

            kernel = np.pad(kernel, pad_width=padding_width, mode='constant', constant_values=0)
        
        super().__init__(cv2.GaussianBlur(kernel, (gaussian_kernel_size, gaussian_kernel_size), gaussian_sigma))


class Square(Kernel):
    def __init__(self, side_length, *, gaussian_sigma=0.5, gaussian_kernel_size=5):
        if side_length == 1:
            kernel = np.ones((1,1))
            print(kernel)
            kernel = kernel.astype(np.float64)
        else:
            kernel = cv2.getStructuringElement(cv2.MORPH_RECT, (side_length, side_length))
            kernel = kernel.astype(np.float64)

        if gaussian_sigma != 0:
            kernel_radius = (gaussian_kernel_size - 1) / 2
            truncate = kernel_radius / gaussian_sigma
            padding_width = ceil(truncate * gaussian_sigma)

            kernel = np.pad(kernel, pad_width=padding_width, mode='constant', constant_values=0)
        
        super().__init__(cv2.GaussianBlur(kernel, (gaussian_kernel_size, gaussian_kernel_size), gaussian_sigma))

class KernelVisualizer:
    def __init__(self):
        self.kernels = {}
    
    def add_kernel(self, name: str, kernel: Kernel):
        """Add a kernel to the visualizer"""
        self.kernels[name] = kernel
    
    def visualize_kernels(self, figsize=(15, 5)):
        """Display all kernels in a grid"""
        if not self.kernels:
            print("No kernels to display. Add kernels first.")
            return
        
        num_kernels = len(self.kernels)
        fig, axes = plt.subplots(1, num_kernels, figsize=figsize)
        
        if num_kernels == 1:
            axes = [axes]
        
        for idx, (name, kernel) in enumerate(self.kernels.items()):
            im = axes[idx].imshow(kernel.grid, cmap='hot', interpolation='nearest')
            axes[idx].set_title(f'{name}\nShape: {kernel.grid.shape}')
            axes[idx].axis('off')
            plt.colorbar(im, ax=axes[idx], fraction=0.046, pad=0.04)
        
        plt.tight_layout()
        plt.show()
    
    def show_kernel_stats(self):
        """Print statistics for all kernels"""
        for name, kernel in self.kernels.items():
            print(f"\n{name}:")
            print(f"  Shape: {kernel.grid.shape}")
            print(f"  Min value: {kernel.grid.min():.6f}")
            print(f"  Max value: {kernel.grid.max():.6f}")
            print(f"  Sum: {kernel.grid.sum():.6f}")
            print(f"  Non-zero elements: {np.count_nonzero(kernel.grid)}")
    
    def test_kernel_operations(self):
        """Test kernel addition and subtraction operations"""
        if len(self.kernels) < 2:
            print("Need at least 2 kernels to test operations.")
            return
        
        kernel_list = list(self.kernels.items())
        kernel1_name, kernel1 = kernel_list[0]
        kernel2_name, kernel2 = kernel_list[1]
        
        print(f"Testing operations between {kernel1_name} and {kernel2_name}")
        
        # Test addition
        try:
            added = kernel1 + kernel2
            print(f"Addition successful: result shape {added.grid.shape}")
            
            # Visualize operation
            fig, axes = plt.subplots(1, 4, figsize=(20, 5))
            
            im1 = axes[0].imshow(kernel1.grid, cmap='hot')
            axes[0].set_title(kernel1_name)
            axes[0].axis('off')
            plt.colorbar(im1, ax=axes[0])
            
            im2 = axes[1].imshow(kernel2.grid, cmap='hot')
            axes[1].set_title(kernel2_name)
            axes[1].axis('off')
            plt.colorbar(im2, ax=axes[1])
            
            im3 = axes[2].imshow(added.grid, cmap='hot')
            axes[2].set_title(f'{kernel1_name} + {kernel2_name}')
            axes[2].axis('off')
            plt.colorbar(im3, ax=axes[2])
            
            # Test subtraction
            subtracted = kernel1 - kernel2
            im4 = axes[3].imshow(subtracted.grid, cmap='hot')
            axes[3].set_title(f'{kernel1_name} - {kernel2_name}')
            axes[3].axis('off')
            plt.colorbar(im4, ax=axes[3])
            
            plt.tight_layout()
            plt.show()
            
        except Exception as e:
            print(f"Operation failed: {e}")
    
    def clear_kernels(self):
        """Clear all kernels from the visualizer"""
        self.kernels.clear()


def demo_kernels():
    """Demonstrate the kernel classes with visualization"""
    print("Creating Kernel Visualizer Demo...")
    
    # Create visualizer
    viz = KernelVisualizer()
    
    # Create different kernel shapes
    print("Creating kernels...")
    disk = Disk(radius=10, gaussian_sigma=0.8, gaussian_kernel_size=5)
    ring = Ring(outer_radius=12, inner_radius=6, gaussian_sigma=0.6, gaussian_kernel_size=7)
    square = Square(side_length=20, gaussian_sigma=0.7, gaussian_kernel_size=5)

    complex_one = Disk(radius=20, gaussian_sigma=0.4) + Ring(outer_radius=100, inner_radius=80, gaussian_kernel_size=101, gaussian_sigma=100) - Square(side_length=10)
    
    # Add to visualizer
    viz.add_kernel("Disk (r=10)", disk)
    viz.add_kernel("Ring (12,6)", ring)
    viz.add_kernel("Square (20x20)", square)
    viz.add_kernel("Complex", complex_one)
    
    # Show statistics
    viz.show_kernel_stats()
    
    # Visualize all kernels
    print("\nVisualizing kernels...")
    viz.visualize_kernels()
    
    # Test operations
    print("\nTesting kernel operations...")
    viz.test_kernel_operations()
    
    return viz


if __name__ == "__main__":
    # Run the demo
    visualizer = demo_kernels()
    
    print("\nDemo complete! You can use the visualizer object to:")
    print("- visualizer.add_kernel('name', kernel_object)")
    print("- visualizer.visualize_kernels()")
    print("- visualizer.show_kernel_stats()")
    print("- visualizer.test_kernel_operations()")