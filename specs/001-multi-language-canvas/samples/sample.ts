/**
 * Sample TypeScript file for testing Living Canvas multi-language support.
 *
 * This file contains typed function patterns to test tree-sitter
 * based call graph extraction.
 */

// Type definitions
interface User {
    name: string;
    email: string;
    age: number;
}

interface ValidationResult {
    valid: boolean;
    errors: string[];
}

type TransformFn<T, U> = (input: T) => U;

// Regular function declarations with types
function greet(name: string): string {
    return `Hello, ${name}!`;
}

function farewell(name: string): string {
    return `Goodbye, ${name}!`;
}

function processName(name: string): string {
    const greeting = greet(name);
    const goodbye = farewell(name);
    return `${greeting} ... ${goodbye}`;
}

// Arrow functions with types
const validateUser = (user: User): ValidationResult => {
    const errors: string[] = [];

    if (!user.name) {
        errors.push('Name is required');
    }
    if (!user.email || !user.email.includes('@')) {
        errors.push('Valid email is required');
    }
    if (user.age < 0 || user.age > 150) {
        errors.push('Age must be between 0 and 150');
    }

    return {
        valid: errors.length === 0,
        errors
    };
};

const transformUser = (user: User): Record<string, string> => {
    const validation = validateUser(user);
    if (!validation.valid) {
        throw new Error(validation.errors.join(', '));
    }
    return {
        NAME: user.name.toUpperCase(),
        EMAIL: user.email.toLowerCase(),
        AGE: String(user.age)
    };
};

const saveUser = (data: Record<string, string>): boolean => {
    console.log('Saving user:', data);
    return true;
};

// Generic functions
function processEntity<T, U>(
    entity: T,
    validator: (e: T) => ValidationResult,
    transformer: TransformFn<T, U>
): U | null {
    const validation = validator(entity);
    if (!validation.valid) {
        console.error('Validation failed:', validation.errors);
        return null;
    }
    return transformer(entity);
}

// Class with typed methods
class UserService {
    private users: User[] = [];

    constructor(initialUsers: User[] = []) {
        this.users = initialUsers;
    }

    addUser(user: User): boolean {
        const validation = validateUser(user);
        if (!validation.valid) {
            return false;
        }
        this.users.push(user);
        return true;
    }

    findUser(name: string): User | undefined {
        return this.users.find(u => u.name === name);
    }

    removeUser(name: string): boolean {
        const index = this.users.findIndex(u => u.name === name);
        if (index === -1) return false;
        this.users.splice(index, 1);
        return true;
    }

    getAllUsers(): User[] {
        return [...this.users];
    }

    processAllUsers(): Record<string, string>[] {
        return this.users
            .filter(u => validateUser(u).valid)
            .map(u => transformUser(u));
    }
}

function useUserService(): void {
    const service = new UserService();

    service.addUser({ name: 'Alice', email: 'alice@example.com', age: 30 });
    service.addUser({ name: 'Bob', email: 'bob@example.com', age: 25 });

    const alice = service.findUser('Alice');
    if (alice) {
        console.log('Found:', greet(alice.name));
    }

    const processed = service.processAllUsers();
    processed.forEach(u => saveUser(u));
}

// Recursive functions with types
function fibonacci(n: number): number {
    if (n <= 1) return n;
    return fibonacci(n - 1) + fibonacci(n - 2);
}

function factorial(n: number): number {
    if (n <= 1) return 1;
    return n * factorial(n - 1);
}

const mathOperations = (x: number, y: number): number => {
    const fibX = fibonacci(x);
    const factY = factorial(y);
    return fibX + factY;
};

// Async functions
async function fetchUserData(id: string): Promise<User> {
    // Simulated async operation
    return new Promise(resolve => {
        setTimeout(() => {
            resolve({
                name: `User${id}`,
                email: `user${id}@example.com`,
                age: 25 + parseInt(id)
            });
        }, 100);
    });
}

async function processUserAsync(id: string): Promise<void> {
    const user = await fetchUserData(id);
    const validation = validateUser(user);

    if (validation.valid) {
        const transformed = transformUser(user);
        saveUser(transformed);
    }
}

// Main entry point
function main(): void {
    // Test greeting functions
    const message = processName('TypeScript');
    console.log(message);

    // Test user service
    useUserService();

    // Test math operations
    const mathResult = mathOperations(10, 5);
    console.log('Math result:', mathResult);

    // Test async processing
    processUserAsync('1').then(() => {
        console.log('Async processing complete');
    });
}

// Export for module usage
export {
    User,
    ValidationResult,
    greet,
    farewell,
    processName,
    validateUser,
    transformUser,
    saveUser,
    processEntity,
    UserService,
    useUserService,
    fibonacci,
    factorial,
    mathOperations,
    fetchUserData,
    processUserAsync,
    main
};

main();
