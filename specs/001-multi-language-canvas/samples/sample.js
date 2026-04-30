/**
 * Sample JavaScript file for testing Living Canvas multi-language support.
 *
 * This file contains various function patterns to test tree-sitter
 * based call graph extraction.
 */

// Regular function declarations
function greet(name) {
    return `Hello, ${name}!`;
}

function farewell(name) {
    return `Goodbye, ${name}!`;
}

function processName(name) {
    const greeting = greet(name);
    const goodbye = farewell(name);
    return `${greeting} ... ${goodbye}`;
}

// Arrow functions
const validateInput = (data) => {
    if (!data) return false;
    if (typeof data !== 'object') return false;
    return true;
};

const transformData = (data) => {
    if (!validateInput(data)) return null;
    return Object.fromEntries(
        Object.entries(data).map(([k, v]) => [k.toUpperCase(), v])
    );
};

const saveResult = (result) => {
    console.log('Saving:', result);
    return true;
};

// Mixed function styles
function processPipeline(data) {
    if (!validateInput(data)) {
        console.log('Invalid input');
        return null;
    }

    const transformed = transformData(data);
    if (transformed) {
        saveResult(transformed);
    }
    return transformed;
}

// Class with methods
class Calculator {
    constructor(initialValue = 0) {
        this.value = initialValue;
    }

    add(x) {
        this.value += x;
        return this;
    }

    subtract(x) {
        this.value -= x;
        return this;
    }

    multiply(x) {
        this.value *= x;
        return this;
    }

    divide(x) {
        if (x !== 0) {
            this.value /= x;
        }
        return this;
    }

    reset() {
        this.value = 0;
        return this;
    }

    getValue() {
        return this.value;
    }
}

function useCalculator() {
    const calc = new Calculator(10);
    calc.add(5).multiply(2).subtract(3);
    return calc.getValue();
}

// Recursive functions
function fibonacci(n) {
    if (n <= 1) return n;
    return fibonacci(n - 1) + fibonacci(n - 2);
}

function factorial(n) {
    if (n <= 1) return 1;
    return n * factorial(n - 1);
}

const mathOperations = (x, y) => {
    const fibX = fibonacci(x);
    const factY = factorial(y);
    return fibX + factY;
};

// Main entry point
function main() {
    // Test greeting functions
    const message = processName('World');
    console.log(message);

    // Test data pipeline
    const data = { name: 'test', value: 42 };
    const result = processPipeline(data);
    console.log('Pipeline result:', result);

    // Test calculator
    const calcResult = useCalculator();
    console.log('Calculator result:', calcResult);

    // Test math operations
    const mathResult = mathOperations(10, 5);
    console.log('Math result:', mathResult);
}

// Export for module usage
if (typeof module !== 'undefined') {
    module.exports = {
        greet,
        farewell,
        processName,
        validateInput,
        transformData,
        processPipeline,
        Calculator,
        useCalculator,
        fibonacci,
        factorial,
        mathOperations,
        main
    };
}

main();
