"""
Sample Python file for testing Living Canvas multi-language support.

This file contains various function definitions and call patterns
to test the tree-sitter based call graph extraction.
"""


def greet(name):
    """Return a greeting message."""
    return f"Hello, {name}!"


def farewell(name):
    """Return a farewell message."""
    return f"Goodbye, {name}!"


def process_name(name):
    """Process a name by greeting and then saying farewell."""
    greeting = greet(name)
    goodbye = farewell(name)
    return f"{greeting} ... {goodbye}"


def validate_input(data):
    """Validate input data."""
    if not data:
        return False
    if not isinstance(data, dict):
        return False
    return True


def transform_data(data):
    """Transform data into a new format."""
    if not validate_input(data):
        return None
    return {k.upper(): v for k, v in data.items()}


def save_result(result):
    """Save result to storage (mock)."""
    print(f"Saving: {result}")
    return True


def process_pipeline(data):
    """Main processing pipeline."""
    if not validate_input(data):
        print("Invalid input")
        return None

    transformed = transform_data(data)
    if transformed:
        save_result(transformed)
    return transformed


class Calculator:
    """Simple calculator class."""

    def __init__(self, initial_value=0):
        """Initialize calculator with a value."""
        self.value = initial_value

    def add(self, x):
        """Add x to current value."""
        self.value += x
        return self

    def subtract(self, x):
        """Subtract x from current value."""
        self.value -= x
        return self

    def multiply(self, x):
        """Multiply current value by x."""
        self.value *= x
        return self

    def divide(self, x):
        """Divide current value by x."""
        if x != 0:
            self.value /= x
        return self

    def reset(self):
        """Reset value to zero."""
        self.value = 0
        return self

    def get_value(self):
        """Get current value."""
        return self.value


def use_calculator():
    """Demonstrate calculator usage."""
    calc = Calculator(10)
    calc.add(5).multiply(2).subtract(3)
    return calc.get_value()


def fibonacci(n):
    """Calculate nth Fibonacci number recursively."""
    if n <= 1:
        return n
    return fibonacci(n - 1) + fibonacci(n - 2)


def factorial(n):
    """Calculate factorial of n."""
    if n <= 1:
        return 1
    return n * factorial(n - 1)


def math_operations(x, y):
    """Perform various math operations."""
    fib_x = fibonacci(x)
    fact_y = factorial(y)
    return fib_x + fact_y


def main():
    """Main entry point."""
    # Test greeting functions
    message = process_name("World")
    print(message)

    # Test data pipeline
    data = {"name": "test", "value": 42}
    result = process_pipeline(data)
    print(f"Pipeline result: {result}")

    # Test calculator
    calc_result = use_calculator()
    print(f"Calculator result: {calc_result}")

    # Test math operations
    math_result = math_operations(10, 5)
    print(f"Math result: {math_result}")


if __name__ == "__main__":
    main()
