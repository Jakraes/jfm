# jfm - JSON Query Language Interpreter

`jfm` is a powerful command-line tool for querying and transforming JSON data using a custom query language. It provides a rich set of features including filtering, mapping, aggregation, and control flow constructs.

## Table of Contents

- [Installation](#installation)
- [Usage](#usage)
- [Query Language](#query-language)
  - [Basic Syntax](#basic-syntax)
  - [Data Types](#data-types)
  - [Variables](#variables)
  - [Field Access](#field-access)
  - [Array Operations](#array-operations)
  - [Operators](#operators)
  - [Control Flow](#control-flow)
  - [Built-in Functions](#built-in-functions)
    - [Array Functions](#array-functions)
    - [Script Functions](#script-functions)
    - [I/O Functions](#io-functions)
  - [Pipe Operator](#pipe-operator)
- [Examples](#examples)

## Installation

Build the project using Cargo:

```bash
cargo build --release
```

The binary will be available at `target/release/jfm.exe` (Windows) or `target/release/jfm` (Unix-like systems).

## Usage

The `jfm` tool accepts JSON input and a query string, then executes the query and outputs the result as JSON. If no query is provided via command-line options, the tool enters interactive mode where you can type out your query over multiple lines.

### Command-Line Options

```
jfm [OPTIONS] [JSON]

Arguments:
  [JSON]    JSON string to process (conflicts with --file)

Options:
  -f, --file <FILE>          Path to JSON file
  -q, --query <QUERY>        Query string to execute
      --query-file <PATH>    Path to file containing query (conflicts with --query)
  -o, --out <OUTPUT_FILE>    Path to output file (default: stdout)
  -h, --help                 Print help
```

### Interactive Mode

When no `--query` or `--query-file` is provided, `jfm` enters interactive mode. This allows you to type your query over multiple lines:

1. Start interactive mode by providing JSON input but no query:
   ```bash
   jfm --file data.json
   ```

2. Type your query line by line. Each line will show a `jfm>` prompt:
   ```bash
   jfm> let filtered = root.users | .age > 25;
   jfm> filtered | .name;
   ```

3. Exit the editor to execute the query:
   - Press **Ctrl+D** (Unix/Linux/macOS) or **Ctrl+Z** (Windows) to exit and run the query
   - Or type `exit` or `quit` on a new line

4. The query executes once after you exit, and the program terminates.

**Example Interactive Session:**

```bash
$ jfm --file data.json
jfm Interactive Query Editor
Type your query (multi-line supported). Exit with Ctrl+D (Ctrl+Z on Windows) or type 'exit' on a new line.

jfm> let users = root.users | .age > 25;
jfm> users | .name;
jfm> exit
["Bob", "Charlie"]
```

### Examples

**Using a JSON file and inline query:**

```bash
jfm --file data.json --query "root.users[0].name"
```

**Using JSON string directly:**

```bash
jfm '{"users": [{"name": "Alice", "age": 25}]}' --query "root.users | .age > 20"
```

**Using query file:**

```bash
jfm --file data.json --query-file query.jfm
```

**Saving output to a file:**

```bash
jfm --file data.json --query "root.users | .name" --out results.json
```

**Interactive mode with output file:**

```bash
jfm --file data.json --out results.json
# Type your query interactively
# Results will be written to results.json
```

## Query Language

### Basic Syntax

The query language uses statements terminated by semicolons (`;`). The last expression's value is returned as the result.

```jfm
let x = 10;
let y = x + 5;
y;
```

### Data Types

The language supports all standard JSON types:

- **Numbers**: `10`, `3.14`, `-5`
- **Strings**: `"hello"`, `"world"`
- **Booleans**: `true`, `false`
- **Null**: `null`
- **Arrays**: `[1, 2, 3]`, `["a", "b", "c"]`
- **Objects**: `{"name": "Alice", "age": 30}`

### Variables

Variables are declared using the `let` keyword:

```jfm
let name = "Alice";
let age = 30;
let numbers = [1, 2, 3, 4, 5];
```

Variables can be reassigned:

```jfm
let x = 10;
x = 20;
x;
```

### Field Access

Access object fields using the dot operator (`.`):

```jfm
root.user.name
root.data.items[0].title
```

**Optional chaining** (`?.`) returns `null` if any part of the chain is null:

```jfm
root?.missing?.field  // Returns null if any part is missing
root.user?.name       // Safe access
```

### Array Operations

**Indexing:**

```jfm
root.items[0]      // First element
root.items[1]      // Second element
root.users[0].name // Access field of array element
```

**Array length:**

```jfm
root.items.length  // Number of elements
```

**Array literals:**

```jfm
let numbers = [1, 2, 3, 4, 5];
let names = ["Alice", "Bob", "Charlie"];
let mixed = [1, "two", true, null];
```

**Object literals:**

```jfm
let user = {"name": "Alice", "age": 30, "active": true};
let nested = {"user": {"name": "Bob", "details": {"age": 25}}};
```

### Operators

#### Arithmetic Operators

```jfm
let sum = 10 + 5;        // 15
let diff = 10 - 5;       // 5
let product = 10 * 5;    // 50
let quotient = 10 / 5;   // 2
let remainder = 10 % 3;  // 1
let power = 2 ^ 3;       // 8 (exponentiation)
```

#### Comparison Operators

```jfm
let eq = 10 == 10;       // true
let ne = 10 != 5;        // true
let gt = 10 > 5;         // true
let lt = 5 < 10;         // true
let ge = 10 >= 10;       // true
let le = 5 <= 10;        // true
```

#### Logical Operators

```jfm
let and = true && false; // false
let or = true || false;  // true
let not = !false;        // true
```

#### Compound Assignment Operators

```jfm
let x = 10;
x += 5;   // x = 15
x -= 3;   // x = 12
x *= 2;   // x = 24
x /= 4;   // x = 6
```

#### String Concatenation

```jfm
let fullName = "John" + " " + "Doe";  // "John Doe"
```

#### Array Concatenation

```jfm
let combined = [1, 2] + [3, 4];  // [1, 2, 3, 4]
```

#### Range Operator

Generate arrays of consecutive numbers:

```jfm
let range1 = 1..5;    // [1, 2, 3, 4, 5]
let range2 = 0..10;   // [0, 1, 2, ..., 10]
let negative = -3..0; // [-3, -2, -1, 0]
```

### Control Flow

#### If-Else Statements

```jfm
if age >= 18 {
    "adult";
} else {
    "minor";
}
```

Chained if-else:

```jfm
if score >= 90 {
    "A";
} else if score >= 80 {
    "B";
} else if score >= 70 {
    "C";
} else {
    "F";
}
```

#### For Loops

Iterate over arrays:

```jfm
let names = [];
for user in root.users {
    names += [user.name];
}
names;
```

Modify elements in a loop:

```jfm
for e in root.employees {
    e.salary = e.salary * 1.10;
}
root.employees;
```

#### Blocks

Code blocks create new scopes:

```jfm
let x = 1;
{
    let x = 2;
    x;  // 2
}
x;  // 1 (outer scope)
```

#### Return Statements

```jfm
if condition {
    return value;
}
// Continue execution...
```

### Built-in Functions

#### Array Functions

**`count(array)`** - Returns the number of elements in an array:

```jfm
count(root.users);  // Number of users
```

**`sum(array)`** - Sums all numeric values in an array:

```jfm
sum([1, 2, 3, 4, 5]);  // 15
sum(root.users | .age);  // Sum of all user ages
```

**`avg(array)`** - Calculates the average of numeric values:

```jfm
avg([10, 20, 30]);  // 20
avg(root.users | .age);  // Average age
```

**`min(array)`** - Returns the minimum numeric value:

```jfm
min([5, 2, 8, 1]);  // 1
min(root.users | .age);  // Minimum age
```

**`max(array)`** - Returns the maximum numeric value:

```jfm
max([5, 2, 8, 1]);  // 8
max(root.users | .age);  // Maximum age
```

**`take(array, count)`** - Returns the first `count` elements:

```jfm
take([1, 2, 3, 4, 5], 3);  // [1, 2, 3]
take(sort_by(root.users, "age"), 5);  // Top 5 users by age
```

**`unique(array)`** - Removes duplicate values:

```jfm
unique([1, 2, 2, 3, 3, 3]);  // [1, 2, 3]
```

**`push(array, value)`** - Adds an element to the end of an array:

```jfm
let items = [1, 2];
items = push(items, 3);  // [1, 2, 3]
```

**`sort_by(array, field)`** - Sorts an array of objects by a field:

```jfm
sort_by(root.users, "age");  // Sort users by age (ascending)
sort_by(root.users, "name");  // Sort users by name
```

**`group_by(array, field)`** - Groups array elements by a field value:

```jfm
group_by(root.users, "age");  // Groups users by age
// Returns: {"25": [...], "30": [...], "35": [...]}
```

#### Script Functions

**`include(path)`** - Executes an external script file and returns its result:

The `include` function allows you to modularize your queries by executing external script files. The included script has access to all variables in the current scope, including `root`, and can return any value.

```jfm
// Execute a script and assign its result to a variable
let result = include("scripts/calculate.jfm");

// Use the result in further computations
result * 2;
```

**Basic Usage:**

```jfm
// File: filter_adults.jfm
let adults = root.users | .age >= 18;
adults;

// Main query
let adult_users = include("filter_adults.jfm");
count(adult_users);
```

**Script with Shared Variables:**

Included scripts can access variables defined in the parent scope:

```jfm
// Main query
let multiplier = 10;
let result = include("compute.jfm");  // Script can access 'multiplier'
result;

// File: compute.jfm
let base = 5;
base * multiplier;  // Returns 50
```

**Nested Includes:**

Scripts can include other scripts, enabling complex modular architectures:

```jfm
// File: utils.jfm
let helper_value = 100;
helper_value;

// File: main_logic.jfm
let from_utils = include("utils.jfm");
from_utils * 2;  // Returns 200

// Main query
include("main_logic.jfm");  // Returns 200
```

**Returning Complex Data:**

Included scripts can return any data type:

```jfm
// File: aggregate.jfm
let ages = root.users | .age;
let stats = {"average": avg(ages), "total": sum(ages), "count": count(ages)};
stats;

// Main query
let statistics = include("aggregate.jfm");
statistics.average;
```

**Error Handling:**

- If the file does not exist, an error is returned
- If the script contains syntax errors, a parse error is returned
- If the script encounters runtime errors, those are propagated

#### I/O Functions

**`print(args...)`** - Prints values to standard output:

The `print` function outputs one or more values to stdout, separated by spaces, followed by a newline. It works similarly to Python's `print()` function.

```jfm
print("Hello, World!");              // Output: Hello, World!
print("Name:", root.name);           // Output: Name: Alice
print("Values:", 1, 2, 3);           // Output: Values: 1 2 3
print();                             // Output: (empty line)
```

**Printing Different Types:**

```jfm
// Strings - printed without quotes
print("Hello");                      // Output: Hello

// Numbers - printed as-is
print(42);                           // Output: 42
print(3.14);                         // Output: 3.14

// Booleans
print(true, false);                  // Output: true false

// Null
print(null);                         // Output: null

// Arrays - printed in bracket notation
print([1, 2, 3]);                    // Output: [1, 2, 3]

// Objects - printed in brace notation
let obj = {"x": 1, "y": 2};
print(obj);                          // Output: {"x": 1, "y": 2}
```

**Debugging with print:**

```jfm
for user in root.users {
    print("Processing:", user.name, "age:", user.age);
    // ... processing logic
}
```

**Return Value:**

`print` always returns `null`, so it can be used as a statement without affecting the result:

```jfm
let result = root.users | .age > 25;
print("Filtered count:", count(result));
result;  // This is the actual return value
```

---

**`input(prompt?)`** - Reads a line of input from standard input:

The `input` function reads a line from stdin and returns it as a string. An optional prompt message can be displayed before reading.

```jfm
let name = input("Enter your name: ");
print("Hello,", name);

let age = input("Enter your age: ");
// Note: input always returns a string
```

**Without Prompt:**

```jfm
let value = input();  // Waits for input without displaying a prompt
```

**Interactive Scripts:**

```jfm
let filename = input("Enter filename to process: ");
let data = include(filename);
print("Loaded", count(data), "records");
```

**Note:** The `input` function is primarily useful in interactive mode or when running jfm scripts that require user interaction. In automated/batch processing scenarios, consider passing data through the JSON input instead.

### Pipe Operator

The pipe operator (`|`) is a powerful feature for filtering and transforming arrays. It passes each element of the array through the expression on the right.

#### Filtering

When the right-hand expression evaluates to a boolean, it filters the array:

```jfm
root.users | .age > 25;  // Users older than 25
root.users | .active == true;  // Active users
root.users | .age > 30 && .active == true;  // Active users over 30
```

#### Mapping

When the right-hand expression returns a value, it maps over the array:

```jfm
root.users | .name;  // Extract all names
root.users | .age * 2;  // Double all ages
root.users | {"name": .name, "age": .age};  // Transform to new objects
```

#### Field Access Shorthand

Inside pipe expressions, you can use `.field` as shorthand for the current item:

```jfm
root.users | .name;  // Equivalent to: root.users | _it.name
```

#### Complex Pipe Expressions

```jfm
root.users | .name == "Bob" || .name == "Alice";  // Users named Bob or Alice
root.users | .age > 25 | .name;  // Names of users over 25
```

## Examples

### Example 1: Simple Field Access

**Input JSON:**
```json
{
  "name": "Alice",
  "age": 30,
  "city": "New York"
}
```

**Query:**
```jfm
root.name;
```

**Output:**
```json
"Alice"
```

### Example 2: Filtering Users

**Input JSON:**
```json
{
  "users": [
    {"name": "Bob", "age": 30},
    {"name": "Alice", "age": 25},
    {"name": "Charlie", "age": 35}
  ]
}
```

**Query:**
```jfm
root.users | .age > 28;
```

**Output:**
```json
[{"name": "Bob", "age": 30}, {"name": "Charlie", "age": 35}]
```

### Example 3: Extracting Names

**Query:**
```jfm
root.users | .name;
```

**Output:**
```json
["Bob", "Alice", "Charlie"]
```

### Example 4: Conditional Logic

**Query:**
```jfm
let results = [];
for u in root.users {
    if u.age > 30 {
        results += ["Senior"];
    } else {
        results += ["Junior"];
    }
}
results;
```

**Output:**
```json
["Junior", "Junior", "Senior"]
```

### Example 5: Aggregation

**Query:**
```jfm
avg(root.users | .age);
```

**Output:**
```json
30
```

### Example 6: Complex Filtering

**Query:**
```jfm
root.users | .name == "Bob" || .name == "Alice";
```

**Output:**
```json
[{"name": "Bob", "age": 30}, {"name": "Alice", "age": 25}]
```

### Example 7: Sorting

**Query:**
```jfm
sort_by(root.users, "age");
```

**Output:**
```json
[
  {"name": "Alice", "age": 25},
  {"name": "Bob", "age": 30},
  {"name": "Charlie", "age": 35}
]
```

### Example 8: Grouping

**Query:**
```jfm
group_by(root.users, "age");
```

**Output:**
```json
{
  "25": [{"name": "Alice", "age": 25}],
  "30": [{"name": "Bob", "age": 30}],
  "35": [{"name": "Charlie", "age": 35}]
}
```

### Example 9: Transforming Data

**Query:**
```jfm
for u in root.users {
    u.status = if u.age > 30 { "Senior" } else { "Junior" };
}
root.users;
```

**Input JSON:**
```json
{
  "users": [
    {"name": "Bob", "age": 30},
    {"name": "Alice", "age": 25},
    {"name": "Charlie", "age": 35}
  ]
}
```

**Output:**
```json
[
  {"name": "Bob", "age": 30, "status": "Junior"},
  {"name": "Alice", "age": 25, "status": "Junior"},
  {"name": "Charlie", "age": 35, "status": "Senior"}
]
```

### Example 10: Nested Field Access

**Input JSON:**
```json
{
  "company": {
    "employees": [
      {"name": "Alice", "department": {"name": "Engineering", "floor": 3}},
      {"name": "Bob", "department": {"name": "Sales", "floor": 2}}
    ]
  }
}
```

**Query:**
```jfm
root.company.employees | .department.name;
```

**Output:**
```json
["Engineering", "Sales"]
```

### Example 11: Multiple Operations

**Query:**
```jfm
let topUsers = take(sort_by(root.users, "age"), 2);
topUsers | .name;
```

**Output:**
```json
["Alice", "Bob"]
```

### Example 12: Working with Numbers

**Query:**
```jfm
let ages = root.users | .age;
let total = sum(ages);
let average = avg(ages);
let minAge = min(ages);
let maxAge = max(ages);
{"total": total, "average": average, "min": minAge, "max": maxAge};
```

**Output:**
```json
{"total": 90, "average": 30, "min": 25, "max": 35}
```

### Example 13: String Operations

**Input JSON:**
```json
{
  "firstName": "John",
  "lastName": "Doe"
}
```

**Query:**
```jfm
root.firstName + " " + root.lastName;
```

**Output:**
```json
"John Doe"
```

### Example 14: Array Manipulation

**Query:**
```jfm
let numbers = [1, 2, 3];
numbers = push(numbers, 4);
numbers = push(numbers, 5);
numbers;
```

**Output:**
```json
[1, 2, 3, 4, 5]
```

### Example 15: Complex Data Transformation

**Input JSON:**
```json
{
  "products": [
    {"name": "Laptop", "price": 999, "category": "Electronics"},
    {"name": "Desk", "price": 299, "category": "Furniture"},
    {"name": "Mouse", "price": 29, "category": "Electronics"}
  ]
}
```

**Query:**
```jfm
let electronics = root.products | .category == "Electronics";
let total = sum(electronics | .price);
let count = count(electronics);
{"count": count, "total": total, "average": total / count};
```

**Output:**
```json
{"count": 2, "total": 1028, "average": 514}
```

### Example 16: Using Range Operator

**Query:**
```jfm
let squares = [];
for i in 1..10 {
    squares += [i * i];
}
squares;
```

**Output:**
```json
[1, 4, 9, 16, 25, 36, 49, 64, 81, 100]
```

### Example 17: Optional Chaining

**Input JSON:**
```json
{
  "user": {
    "name": "Alice"
  }
}
```

**Query:**
```jfm
root.user?.missing?.field;  // Returns null instead of error
```

**Output:**
```json
null
```

### Example 18: Creating New Objects

**Query:**
```jfm
root.users | {"fullName": .name, "isAdult": .age >= 18, "ageGroup": if .age > 30 { "Senior" } else { "Junior" }};
```

**Output:**
```json
[
  {"fullName": "Bob", "isAdult": true, "ageGroup": "Junior"},
  {"fullName": "Alice", "isAdult": true, "ageGroup": "Junior"},
  {"fullName": "Charlie", "isAdult": true, "ageGroup": "Senior"}
]
```

## Error Handling

The tool will exit with an error code if:

- JSON parsing fails
- Query syntax is invalid
- Runtime errors occur (e.g., division by zero, index out of bounds, undefined variables)
- No query is entered in interactive mode

Error messages are printed to stderr, while results are printed to stdout (or written to the output file if `--out` is specified).

## Notes

- The `root` variable contains the input JSON data
- Array indices are zero-based
- All numbers are floating-point (64-bit)
- String comparisons are case-sensitive
- The pipe operator (`|`) provides syntactic sugar for common filtering and mapping operations
- Variables are scoped to blocks (blocks create new scopes)
- The last expression's value is returned as the result

