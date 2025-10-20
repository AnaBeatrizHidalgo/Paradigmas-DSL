import cv2
import yaml
import numpy as np
from typing import Dict, Any, Callable
from Grid import Grid
from Rule import Rule
from kernel import Ring, Disk, Square, Kernel
from LifeForm import LifeForm


class AutomataConfigLoader:
    def __init__(self, config_path: str):
        with open(config_path, 'r') as f:
            self.config = yaml.safe_load(f)
        
        self.grid_size = None
        self.grid = None
        self.functions_cache: Dict[str, Callable] = {}
        self.kernels_cache: Dict[str, Kernel] = {}
    
    def _create_function(self, func_config: Dict[str, Any]) -> Callable[[np.ndarray], np.ndarray]:
        """Create a growth function from configuration."""
        func_type = func_config['type']
        
        if func_type == 'gaussian':
            mu = func_config['mu']
            sigma = func_config['sigma']
            amplitude = func_config.get('amplitude', 2.0)
            baseline = func_config.get('baseline', -1.0)
            
            def gaussian_func(x: np.ndarray) -> np.ndarray:
                diff = (x - mu) / sigma
                return amplitude * np.exp(-(diff**2) / 2) + baseline
            
            return gaussian_func
        
        elif func_type == 'step':
            threshold = func_config['threshold']
            low_value = func_config['low_value']
            high_value = func_config['high_value']
            
            def step_func(x: np.ndarray) -> np.ndarray:
                return np.where(x >= threshold, high_value, low_value)
            
            return step_func
        
        elif func_type == 'linear':
            slope = func_config['slope']
            intercept = func_config['intercept']
            
            def linear_func(x: np.ndarray) -> np.ndarray:
                return slope * x + intercept
            
            return linear_func
        
        elif func_type == 'conway':
            underpop = func_config['underpopulation']
            overpop = func_config['overpopulation']
            reprod = func_config['reproduction']
            
            def conway_func(x: np.ndarray) -> np.ndarray:
                grid = np.zeros_like(x)
                grid = grid - (x < underpop).astype(float)
                grid = grid - (x > overpop).astype(float)
                grid = grid + (x == reprod).astype(float)
                return grid
            
            return conway_func
        
        elif func_type == 'identity':
            return lambda x: x
        
        else:
            raise ValueError(f"Unknown function type: {func_type}")
    
    def _create_kernel(self, kernel_config: Dict[str, Any]) -> Kernel:
        """Create a kernel from configuration."""
        kernel_type = kernel_config['type']
        
        if kernel_type == 'disk':
            radius = kernel_config['radius']
            gaussian_sigma = kernel_config.get('gaussian_sigma', 0.5)
            gaussian_kernel_size = kernel_config.get('gaussian_kernel_size', 5)
            return Disk(radius, gaussian_sigma=gaussian_sigma, 
                       gaussian_kernel_size=gaussian_kernel_size)
        
        elif kernel_type == 'ring':
            outer_diameter = kernel_config['outer_diameter']
            inner_diameter = kernel_config['inner_diameter']
            gaussian_sigma = kernel_config.get('gaussian_sigma', 0.5)
            gaussian_kernel_size = kernel_config.get('gaussian_kernel_size', 5)
            return Ring(outer_diameter, inner_diameter, 
                       gaussian_sigma=gaussian_sigma,
                       gaussian_kernel_size=gaussian_kernel_size)
        
        elif kernel_type == 'square':
            # Check for custom array first
            if 'custom_array' in kernel_config:
                arr = np.array(kernel_config['custom_array'], dtype=np.float64)
                return Kernel(arr)
            
            # Otherwise use side_length
            side_length = kernel_config['side_length']
            gaussian_sigma = kernel_config.get('gaussian_sigma', 0.5)
            gaussian_kernel_size = kernel_config.get('gaussian_kernel_size', 5)
            
            return Square(side_length, gaussian_sigma=gaussian_sigma,
                         gaussian_kernel_size=gaussian_kernel_size)
        
        elif kernel_type == 'composite':
            operation = kernel_config['operation']
            left = self._resolve_kernel(kernel_config['left'])
            right = self._resolve_kernel(kernel_config['right'])
            
            if operation == 'add':
                return left + right
            elif operation == 'subtract':
                return left - right
            elif operation == 'multiply':
                if isinstance(right, (int, float)):
                    return left * right
                else:
                    raise ValueError("Multiply operation requires right operand to be a number")
            elif operation == 'divide':
                if isinstance(right, (int, float)):
                    return left / right
                else:
                    raise ValueError("Divide operation requires right operand to be a number")
            else:
                raise ValueError(f"Unknown composite operation: {operation}")
        
        else:
            raise ValueError(f"Unknown kernel type: {kernel_type}")
    
    def _resolve_kernel(self, kernel_ref: Any) -> Kernel:
        """Resolve a kernel reference (can be a dict config, a ref string, or a number)."""
        if isinstance(kernel_ref, (int, float)):
            return kernel_ref
        
        if isinstance(kernel_ref, dict):
            if 'ref' in kernel_ref:
                # Reference to predefined kernel
                ref_name = kernel_ref['ref']
                if ref_name not in self.kernels_cache:
                    raise ValueError(f"Kernel reference '{ref_name}' not found")
                return self.kernels_cache[ref_name]
            else:
                # Inline kernel definition
                return self._create_kernel(kernel_ref)
        
        raise ValueError(f"Invalid kernel reference: {kernel_ref}")
    
    def _resolve_function(self, func_ref: Any) -> Callable:
        """Resolve a function reference (can be a dict config or a ref string)."""
        if isinstance(func_ref, dict):
            if 'ref' in func_ref:
                # Reference to predefined function
                ref_name = func_ref['ref']
                if ref_name not in self.functions_cache:
                    raise ValueError(f"Function reference '{ref_name}' not found")
                return self.functions_cache[ref_name]
            else:
                # Inline function definition
                return self._create_function(func_ref)
        
        raise ValueError(f"Invalid function reference: {func_ref}")
    
    def _initialize_lifeform_state(self, lifeform: LifeForm, init_type: str):
        """Initialize the state of a lifeform based on configuration."""
        if init_type == 'random':
            lifeform.randomize_state(discrete=False)
        elif init_type == 'random_discrete':
            lifeform.randomize_state(discrete=True)
        elif init_type == 'empty':
            lifeform.state = np.zeros(self.grid_size, dtype=np.float64)
        elif init_type == 'centered_blob':
            h, w = self.grid_size
            lifeform.state = np.zeros(self.grid_size, dtype=np.float64)
            # Create a small blob in the center
            center_h, center_w = h // 2, w // 2
            blob_size = min(h, w) // 10
            y, x = np.ogrid[-blob_size:blob_size, -blob_size:blob_size]
            mask = x*x + y*y <= blob_size*blob_size
            lifeform.state[center_h-blob_size:center_h+blob_size, 
                          center_w-blob_size:center_w+blob_size][mask] = 1.0
        elif init_type == 'random_sparse':
            lifeform.state = np.random.random(self.grid_size)
            lifeform.state = (lifeform.state > 0.95).astype(np.float64)
        else:
            raise ValueError(f"Unknown initialization type: {init_type}")
    
    def load(self) -> Grid:
        """Load the complete configuration and create the Grid."""
        # Parse grid configuration
        grid_config = self.config['grid']
        width = grid_config['width']
        height = grid_config['height']
        self.grid_size = (height, width)
        
        # Cache predefined functions
        if 'functions' in self.config:
            for func_name, func_config in self.config['functions'].items():
                self.functions_cache[func_name] = self._create_function(func_config)
        
        # Cache predefined kernels
        if 'kernels' in self.config:
            for kernel_name, kernel_config in self.config['kernels'].items():
                self.kernels_cache[kernel_name] = self._create_kernel(kernel_config)
        
        # Create grid
        self.grid = Grid(self.grid_size)
        
        # Create lifeforms
        lifeforms_config = self.config['lifeforms']
        lifeforms_dict: Dict[str, LifeForm] = {}
        
        for lifeform_name, lifeform_config in lifeforms_config.items():
            # Create lifeform
            lifeform = LifeForm(lifeform_name, self.grid_size)
            lifeforms_dict[lifeform_name] = lifeform
            
            # Initialize state
            init_type = lifeform_config.get('initial_state', 'random')
            self._initialize_lifeform_state(lifeform, init_type)
            
            # Add rules
            rules_config = lifeform_config['rules']
            for target_name, rule_config in rules_config.items():
                dt = rule_config['dt']
                kernel = self._resolve_kernel(rule_config['kernel'])
                func = self._resolve_function(rule_config['func'])
                weight = rule_config.get('weight', 1.0)
                
                rule = Rule(dt, kernel, func)
                lifeform.add_rule(target_name, rule, weight)
            
            # Add to grid
            color = lifeform_config.get('color', '#FFFFFF')
            self.grid.add_lifeform(lifeform, color)
        
        return self.grid
    
    def get_display_config(self) -> Dict[str, Any]:
        """Get display configuration."""
        return self.config.get('display', {
            'window_name': 'Continuous Automata',
            'quit_key': 'q',
            'show_fps': True,
            'scale': 1.0
        })
    
    def get_simulation_config(self) -> Dict[str, Any]:
        """Get simulation configuration."""
        return self.config.get('simulation', {
            'max_iterations': None,
            'save_every': None,
            'output_directory': './output',
            'record_video': False,
            'video_fps': 30
        })


def run(grid: Grid, display_config: Dict[str, Any]):
    """Run the simulation with the given grid and display configuration."""
    window_name = display_config.get('window_name', 'Continuous Automata')
    quit_key = display_config.get('quit_key', 'q')
    show_fps = display_config.get('show_fps', True)
    scale = display_config.get('scale', 1.0)
    
    import time
    frame_count = 0
    start_time = time.time()
    
    while True:
        # Update state
        grid.update()
        
        # Display
        frame = grid.calculate_displayable_version()
        
        # Scale the frame if needed
        if scale != 1.0:
            new_width = int(frame.shape[1] * scale)
            new_height = int(frame.shape[0] * scale)
            frame = cv2.resize(frame, (new_width, new_height), interpolation=cv2.INTER_NEAREST)
        
        # Show FPS
        if show_fps:
            frame_count += 1
            elapsed = time.time() - start_time
            if elapsed > 0:
                fps = frame_count / elapsed
                cv2.putText(frame, f"FPS: {fps:.1f}", (10, 30),
                           cv2.FONT_HERSHEY_SIMPLEX, 1, (255, 255, 255), 2)
        
        cv2.imshow(window_name, frame)
        
        # Check for quit
        if cv2.waitKey(1) & 0xFF == ord(quit_key):
            break
    
    cv2.destroyAllWindows()


if __name__ == "__main__":
    import sys
    
    # Get config file path from command line or use default
    config_path = sys.argv[1] if len(sys.argv) > 1 else "automata_config.yml"
    
    print(f"Loading configuration from: {config_path}")
    loader = AutomataConfigLoader(config_path)
    
    print("Creating grid and lifeforms...")
    grid = loader.load()
    
    print("Starting simulation...")
    display_config = loader.get_display_config()
    run(grid, display_config)