# jfm - JSON Query & Transform Language

**jfm** is a powerful, expressive command-line tool and domain-specific language for querying, filtering, and transforming JSON data. With a syntax inspired by modern JavaScript and functional programming, jfm makes JSON manipulation intuitive and powerful.

## ✨ Key Features

- 🎯 **Intelligent Pipe Operator** - Filter, map, and transform data with a single `|` operator
- 🔄 **Functional Programming** - First-class functions, lambdas, and higher-order operations
- 📦 **Module System** - Organize code with `import()` and `include()` for reusable libraries
- 🎨 **Template Literals** - String interpolation with `${...}` expressions
- 🧩 **Pattern Matching** - Match expressions for elegant conditional logic
- 🚀 **Generators** - Built-in `range()`, `replicate()`, and `cross()` for data generation
- 🔍 **Optional Chaining** - Safe navigation with `?.` operator
- 🎭 **Spread Operators** - Expand arrays and objects with `...`
- 🔧 **Rich Built-ins** - 50+ functions for arrays, strings, objects, math, and I/O

## 🚀 Quick Start

### Installation

```bash
git clone <repository>
cd jfm-2
cargo build --release
```

The binary will be available at `target/release/jfm`.

### Basic Usage

```bash
# Query a JSON file
jfm -f data.json -q ".users | .age > 25"

# Inline JSON processing
jfm '{"name": "Alice", "age": 30}' -q ".name"

# Interactive mode - explore data step by step
jfm -f data.json

# Stream large files efficiently
jfm --stream --limit 10 -q ".name" -f huge.jsonl

# Save output to file
jfm -f data.json -q ".users | .name" -o names.json
```

## 📖 Language Guide

### Core Concepts

jfm is a functional, expression-based language. The last expression in your script is the return value. If no explicit return is provided, `root` (the input JSON) is returned.

### Data Types

jfm supports all JSON types plus additional features:

```jfm
42, 3.14                    // Numbers (64-bit floats)
"hello", 'single quotes'    // Strings (both quote styles supported)
`Hello ${name}!`            // Template literals with interpolation
true, false                 // Booleans
null                        // Null
[1, 2, 3]                   // Arrays
{"key": "value"}            // Objects
x => x * 2                  // Lambda functions
```

### Variables and Constants

Three ways to declare variables with different mutability rules:

```jfm
x = 10                      // Simple assignment (can reassign)
let y = 20                  // Explicit let declaration (can reassign)
const z = 30                // Constant (immutable, cannot reassign)

x = 30                      // ✅ Works
y = 40                      // ✅ Works
z = 50                      // ❌ Error: cannot reassign const
```

**Note:** Variables declared with `let` or `const` are block-scoped, while simple assignments use function scope.

### Field Access

Multiple ways to access nested data:

```jfm
root.user.name              // Dot notation
root.users[0].name          // Array indexing
root?.optional?.field       // Optional chaining (returns null if missing)
.users                      // Short form (implicit root)
root["key"]                 // Bracket notation for dynamic keys
[1, 2, 3].length            // Array length property
```

**Optional Chaining:** The `?.` operator safely navigates potentially missing properties:
```jfm
user?.address?.city ?? "Unknown"  // Returns "Unknown" if any part is null/undefined
```

### Operators

#### Arithmetic
```jfm
+ - * / % ^                 // Addition, subtraction, multiplication, division, modulo, power
2 ^ 8                       // 256 (exponentiation)
10 % 3                      // 1 (modulo)
```

#### Comparison
```jfm
== != > < >= <=             // Equality, inequality, relational operators
.age > 25
.status == "active"
```

#### Logical
```jfm
&& || !                     // AND, OR, NOT
.active && .verified
!disabled
```

#### Ternary Conditional
```jfm
condition ? true_value : false_value
age >= 18 ? "adult" : "minor"
```

#### Null Coalescing
```jfm
left ?? right               // Returns right if left is null/undefined
user.nickname ?? user.name ?? "Anonymous"
```

#### Range Operator
```jfm
start..end                  // Generates inclusive array [start, start+1, ..., end]
1..5                        // [1, 2, 3, 4, 5]
0..10                       // [0, 1, 2, ..., 10]
```

#### String/Array Concatenation
```jfm
"a" + "b"                   // "ab"
[1, 2] + [3, 4]             // [1, 2, 3, 4]
```

#### Compound Assignment
```jfm
x += 5                      // x = x + 5
x -= 2                      // x = x - 2
x *= 3                      // x = x * 3
x /= 2                      // x = x / 2
```

### Pattern Matching

Match expressions provide elegant pattern-based conditional logic:

```jfm
match value {
    200 => "OK",
    404 => "Not Found",
    500..599 => "Server Error",
    _ => "Unknown"
}
```

Pattern matching supports:
- **Literal values**: `200`, `"ok"`, `true`
- **Range patterns**: `500..599` (matches any number in range)
- **Wildcard**: `_` (matches anything, must be last)

```jfm
match statusCode {
    200 => "Success",
    300..399 => "Redirect",
    400..499 => "Client Error",
    500..599 => "Server Error",
    _ => "Unknown Status"
}
```

### Control Flow

#### If-Else Statements
```jfm
if condition {
    // statements
} else {
    // statements
}
```

Expressions can be used directly:
```jfm
if age >= 18 { "adult" } else { "minor" }
```

#### For Loops
```jfm
for item in array {
    // statements
}

for i in 1..10 {
    if i == 5 { break; }
    if i % 2 == 0 { continue; }
    print(i);
}
```

#### While Loops
```jfm
while condition {
    // statements
}

let count = 0;
while count < 5 {
    print(count);
    count += 1;
}
```

#### Break and Continue
- `break` - Exit the current loop
- `continue` - Skip to next iteration

### Functions

#### Named Functions
```jfm
fn greet(name) {
    return `Hello, ${name}!`;
}

fn factorial(n) {
    if n <= 1 { return 1; }
    return n * factorial(n - 1);
}
```

#### Lambda Functions
```jfm
let double = x => x * 2;
let add = (a, b) => a + b;
let multiply = (a, b) => a * b;
```

#### Short Function Syntax
```jfm
multiply(a, b) => a * b     // Alternative to: let multiply = (a, b) => a * b
```

#### Default Parameters
```jfm
fn greet(name, greeting = "Hello") {
    return `${greeting}, ${name}!`;
}

greet("Alice")              // "Hello, Alice!"
greet("Bob", "Hi")          // "Hi, Bob!"
```

### The Pipe Operator: jfm's Superpower

The `|` (pipe) operator is the heart of jfm. It intelligently handles filtering, mapping, and transformation based on the expression type.

#### Filtering (Boolean Expressions)
```jfm
.users | .age > 25                  // Keep users where age > 25
.users | .active == true            // Filter active users
.users | @.age > 18 && @.verified   // Complex filter with @
```

#### Mapping (Field Access)
```jfm
.users | .name                      // Extract names → ["Alice", "Bob", "Charlie"]
.users | .profile.email             // Extract nested values
```

#### Transformation (Arithmetic Operations)
```jfm
.users | .age * 2                   // Double everyone's age
.users | .salary + 1000             // Give everyone a raise
.users | .score / 10                // Normalize scores
```

#### Nested Field Mutations
```jfm
.users | .profile.age + 5           // Add 5 to nested profile.age
.users | .profile.name = "Updated"  // Set nested field
.users | .items[0] = "first"        // Set array element
```

#### Lambda Expressions in Pipes
```jfm
.users | x => x.name                // Map with named parameter
.users | x => x.age > 25            // Filter with named parameter
.users | x => {
    name: x.name,
    age: x.age * 2
}                                   // Complex transformation
```

#### The @ Variable
Use `@` to explicitly reference the current item:

```jfm
.numbers | @ > 5                    // Filter numbers > 5
.numbers | @ * 2                    // Double each number
.users | @.age > 25 && @.active     // Complex filter
```

#### Named Pipe Context with `as`
Name the current item for clearer code:

```jfm
.users | @ as user | user.name      // Name current item "user"
.cells | @ as cell | 3 * {          // Combine with replication
    ...template,
    cell_id: cell.id,
    index: @                        // @ now refers to replication index
}
```

#### Pipe Chaining
Chain multiple operations:

```jfm
.users | .age > 25 | .name          // Filter adults, then get names
.items | .price > 100 | .category   // Filter expensive, get category
.users | .active | .email | [0]     // Get first active user's email
```

#### Array Indexing in Pipes
```jfm
.users | .active | [0]              // Get first active user
.items | .inStock | [0..2]          // Get first 3 in-stock items
```

#### Function Calls in Pipes
Functions work seamlessly:

```jfm
.numbers | sum                      // Bare function name
.numbers | sum()                    // Explicit call (same result)
.users | sort_by("age")             // With arguments
```

#### Method-Style Calls
Call functions as methods on values:

```jfm
.numbers.sum()                      // Same as sum(.numbers)
.users.sort_by("age")               // Same as sort_by(.users, "age")
"hello".upper()                     // Same as upper("hello")
```

#### Pipe Update Operator
The `|~` operator updates nested fields in place:

```jfm
.users |~ .profile.age + 5          // Update nested field
.config |~ .theme = "dark"          // Set nested value
```

### Template Literals

Template literals enable string interpolation with embedded expressions:

```jfm
let name = "Alice";
let greeting = `Hello, ${name}!`;                    // "Hello, Alice!"
let summary = `Found ${items.length} items`;         // Embedded expressions
let total = `$${sum(items | .price)}`;               // Complex expressions
```

**Multi-line strings:**
```jfm
let html = `
  <div>
    <h1>${title}</h1>
    <p>${description}</p>
  </div>
`;
```

**Escape sequences:**
```jfm
`Line 1\nLine 2`                    // Newline: \n
`Tab:\tvalue`                       // Tab: \t
`Path: C:\\Users`                   // Backslash: \\
`Cost: \$100`                       // Literal $: \$
`Say \`hello\``                     // Backtick: \`
```

### Object and Array Literals

#### Object Literals
```jfm
{                                    
    "name": "Alice",                 // Quoted keys
    age: 30,                         // Unquoted keys (identifier style)
    active: true
}
```

#### Shorthand Properties
```jfm
let name = "Alice";
let age = 30;
{name, age}                          // Same as {name: name, age: age}
```

#### Spread Operator
```jfm
// Object spread
let base = {a: 1, b: 2};
{...base, c: 3}                      // {a: 1, b: 2, c: 3}
{...base, a: 100}                    // {a: 100, b: 2} (override)

// Array spread
let arr = [2, 3];
[1, ...arr, 4]                       // [1, 2, 3, 4]
```

#### Nested Path Keys
Set nested properties directly in object literals:

```jfm
let cell = {name: "cell1", config: {x: 1}};
{...cell, config.y: 2}               // {name: "cell1", config: {x: 1, y: 2}}
```

#### Array Literals
```jfm
[1, 2, 3]
[1, ...otherArray, 4]
[]
```

### Replication Operator

The `*` operator creates multiple copies with index-based generation:

```jfm
3 * {id: @}                         // [{id: 0}, {id: 1}, {id: 2}]
3 * {id: @, name: `Item ${@}`}      // @ is the replication index

// Combine with spread
let template = {type: "item", data: {active: true}};
3 * {...template, data.id: @}       // Create 3 items with unique IDs
```

### Module System

jfm supports two module systems for code organization:

#### Include (Script Execution)
`include()` executes another jfm file and returns its result:

```jfm
// utils.jfm
fn process(data) {
    return data | .active == true | .name;
}

// main.jfm
let result = include("utils.jfm");
process(root.users)
```

#### Import (Module Object)
`import()` loads a module and returns a module object with all functions:

```jfm
// math.jfm
fn add(a, b) {
    return a + b;
}

fn multiply(a, b) {
    return a * b;
}

fn square(x) {
    return x * x;
}
```

```jfm
// main.jfm
let math = import("math.jfm");

// Call module functions using ::
let sum = math::add(5, 3);                          // 8
let product = math::multiply(4, 7);                 // 28
let squared = math::square(6);                      // 36

// Chain module calls
let result = math::add(math::square(3), math::square(4));  // 25
```

**Module features:**
- `import(path)` returns a module object containing all functions
- Use `::` (double colon) to call functions: `module::function()`
- Modules are isolated - internal variables don't leak
- `typeof(module)` returns `"module"`

## 📚 Built-in Functions

### Array Functions

| Function | Description | Example |
|----------|-------------|---------|
| `arr.length` | Get array length | `[1,2,3].length` → `3` |
| `sum(arr)` | Sum all numbers | `sum([1,2,3])` → `6` |
| `avg(arr)` | Calculate average | `avg([1,2,3])` → `2` |
| `min(arr)` | Find minimum | `min([3,1,2])` → `1` |
| `max(arr)` | Find maximum | `max([1,3,2])` → `3` |
| `unique(arr)` | Remove duplicates | `unique([1,1,2])` → `[1,2]` |
| `sort(arr)` | Sort primitives | `sort([3,1,2])` → `[1,2,3]` |
| `sort_by(arr, field)` | Sort by object field | `sort_by(users, "age")` |
| `group_by(arr, field)` | Group by field | `group_by(users, "dept")` |
| `reverse(arr)` | Reverse array | `reverse([1,2,3])` → `[3,2,1]` |
| `slice(arr, start, end?)` | Extract slice | `slice([1,2,3,4], 1, 3)` → `[2,3]` |
| `push(arr, val)` | Append element | `push([1,2], 3)` → `[1,2,3]` |
| `pop(arr)` | Remove last element | `pop([1,2,3])` → `3` |
| `shift(arr)` | Remove first element | `shift([1,2,3])` → `1` |
| `flat(arr, depth?)` | Flatten nested arrays | `flat([[1],[2]])` → `[1,2]` |
| `flat_map(arr, fn)` | Map then flatten | `flat_map([1,2], x => [x, x*10])` |
| `first(arr)` | Get first element | `first([1,2,3])` → `1` |
| `last(arr)` | Get last element | `last([1,2,3])` → `3` |
| `zip(a, b)` | Pair arrays | `zip([1,2],[3,4])` → `[[1,3],[2,4]]` |
| `find(arr, fn)` | Find first match | `find([1,2,3], x => x > 1)` → `2` |
| `find_index(arr, fn)` | Find index of match | `find_index([1,2,3], x => x > 1)` → `1` |
| `every(arr, fn)` | All match predicate? | `every([2,4], x => x % 2 == 0)` → `true` |
| `some(arr, fn)` | Any match predicate? | `some([1,2], x => x > 1)` → `true` |
| `reduce(arr, fn, init)` | Reduce to single value | `reduce([1,2,3], (a,v) => a+v, 0)` → `6` |
| `enumerate(arr)` | Get index-value pairs | `enumerate(["a","b"])` → `[[0,"a"],[1,"b"]]` |
| `clone(val)` | Deep copy | `clone([1,[2,3]])` |

### String Functions

| Function | Description | Example |
|----------|-------------|---------|
| `len(s)` | Get string length | `len("hello")` → `5` |
| `trim(s)` | Remove whitespace | `trim("  hi  ")` → `"hi"` |
| `upper(s)` | Convert to uppercase | `upper("hi")` → `"HI"` |
| `lower(s)` | Convert to lowercase | `lower("HI")` → `"hi"` |
| `split(s, delim)` | Split string | `split("a,b", ",")` → `["a","b"]` |
| `join(arr, delim)` | Join array | `join(["a","b"], ",")` → `"a,b"` |
| `contains(s, sub)` | Check substring | `contains("hello", "ell")` → `true` |
| `starts_with(s, pre)` | Check prefix | `starts_with("hello", "he")` → `true` |
| `ends_with(s, suf)` | Check suffix | `ends_with("hello", "lo")` → `true` |
| `replace(s, from, to)` | Replace substring | `replace("hi", "i", "o")` → `"ho"` |

### Object Functions

| Function | Description | Example |
|----------|-------------|---------|
| `keys(obj)` | Get all keys | `keys({"a":1})` → `["a"]` |
| `values(obj)` | Get all values | `values({"a":1})` → `[1]` |
| `entries(obj)` | Get key-value pairs | `entries({"a":1})` → `[["a",1]]` |
| `has(obj, key)` | Check key existence | `has({"a":1}, "a")` → `true` |
| `merge(o1, o2)` | Shallow merge | `merge({"a":1}, {"b":2})` → `{"a":1,"b":2}` |
| `deep_merge(o1, o2)` | Deep merge nested objects | `deep_merge({a:{x:1}}, {a:{y:2}})` |
| `set_path(obj, path, val)` | Set nested path | `set_path({}, "a.b", 1)` → `{a:{b:1}}` |
| `clone(obj)` | Deep copy object | `clone({a:1})` |

### Type Functions

| Function | Description | Example |
|----------|-------------|---------|
| `typeof(v)` | Get type name | `typeof(42)` → `"number"` |
| `is_null(v)` | Check if null | `is_null(null)` → `true` |
| `is_array(v)` | Check if array | `is_array([])` → `true` |
| `is_object(v)` | Check if object | `is_object({})` → `true` |
| `is_string(v)` | Check if string | `is_string("hi")` → `true` |
| `is_number(v)` | Check if number | `is_number(42)` → `true` |
| `is_bool(v)` | Check if boolean | `is_bool(true)` → `true` |
| `to_string(v)` | Convert to string | `to_string(42)` → `"42"` |
| `to_number(v)` | Convert to number | `to_number("42")` → `42` |
| `to_int(v)` | Convert to integer | `to_int(3.7)` → `3` |
| `to_float(v)` | Convert to float | `to_float(42)` → `42.0` |
| `to_bool(v)` | Convert to boolean | `to_bool(0)` → `false` |
| `parse_json(s)` | Parse JSON string | `parse_json("[1,2]")` → `[1,2]` |

### Math Functions

| Function | Description | Example |
|----------|-------------|---------|
| `floor(n)` | Round down | `floor(3.7)` → `3` |
| `ceil(n)` | Round up | `ceil(3.2)` → `4` |
| `round(n)` | Round to nearest | `round(3.5)` → `4` |
| `abs(n)` | Absolute value | `abs(-5)` → `5` |
| `sqrt(n)` | Square root | `sqrt(16)` → `4` |
| `pow(a, b)` | Exponentiation | `pow(2, 8)` → `256` |
| `sin(n)` | Sine (radians) | `sin(0)` → `0` |
| `cos(n)` | Cosine (radians) | `cos(0)` → `1` |
| `tan(n)` | Tangent (radians) | `tan(0)` → `0` |
| `random()` | Random 0-1 | `random()` → `0.123...` |

### Generator Functions

| Function | Description | Example |
|----------|-------------|---------|
| `replicate(n, fn)` | Generate n items | `replicate(3, i => i * 10)` → `[0,10,20]` |
| `range(start, end, step?)` | Number sequence | `range(0, 10, 2)` → `[0,2,4,6,8,10]` |
| `cross(arr1, arr2, ...)` | Cartesian product | `cross([1,2], ["a","b"])` → `[[1,"a"],[1,"b"],[2,"a"],[2,"b"]]` |

### I/O Functions

| Function | Description | Example |
|----------|-------------|---------|
| `print(args...)` | Print to stdout | `print("Hello", name)` |
| `input(prompt?)` | Read from stdin | `input("Name: ")` |
| `include(path)` | Execute script file | `include("utils.jfm")` |
| `import(path)` | Import module | `let math = import("math.jfm")` |

## 💡 Real-World Examples

### Data Filtering and Extraction

```jfm
// Get names of active users over 25
.users | .active == true && .age > 25 | .name

// Extract emails from verified accounts
.users | .verified == true | .email

// Get first 5 active items
.items | .inStock == true | [0..4]
```

### Data Transformation

```jfm
// Transform user data
.users | {
    fullName: .firstName + " " + .lastName,
    isAdult: .age >= 18,
    category: .age > 30 ? "Senior" : "Junior",
    displayName: .nickname ?? .firstName
}

// Normalize prices (convert cents to dollars)
.products | {
    ...@,
    price: .price / 100,
    priceFormatted: `$${(.price / 100).toFixed(2)}`
}
```

### Aggregations

```jfm
// Calculate statistics
let ages = .users | .age;
{
    count: ages.length,
    average: avg(ages),
    min: min(ages),
    max: max(ages),
    sum: sum(ages)
}

// Group by department
group_by(.employees, "department")

// Top N by score
slice(sort_by(.users, "score"), 0, 10)
```

### Complex Queries

```jfm
// Find all users with matching skills
.users | 
    @ as user |
    user.skills | 
    contains(@, "JavaScript") |
    user.name

// Transform and filter in one pipeline
.items |
    .price > 100 |                    // Filter expensive
    @ as item |
    {                                  // Transform
        name: item.name,
        discountedPrice: item.price * 0.9,
        category: item.category
    } |
    .category == "Electronics"         // Filter by category
```

### Data Generation

```jfm
// Generate test data
let users = replicate(10, i => ({
    id: i,
    name: `User ${i}`,
    email: `user${i}@example.com`,
    age: 20 + (i * 2),
    active: i % 2 == 0
}));

// Generate configuration
let configs = cross(["dev", "prod"], ["us", "eu"]) | 
    @ as pair |
    {
        env: pair[0],
        region: pair[1],
        apiUrl: `https://${pair[0]}.api.${pair[1]}.com`
    }
```

### Working with Nested Data

```jfm
// Extract deeply nested values
.company.employees | .department.location.city

// Safe navigation with defaults
.user?.profile?.settings?.theme ?? "default"

// Update nested structures
.users | .profile.settings.theme = "dark"
```

### Custom Functions

```jfm
// Reusable utility functions
fn format_currency(amount) {
    return `$${amount.toFixed(2)}`;
}

fn is_premium(user) {
    return user.subscription?.tier == "premium" && user.active == true;
}

fn calculate_discount(price, percentage) {
    return price * (1 - percentage / 100);
}

// Use in pipelines
.users | x => is_premium(x) | {
    name: x.name,
    discount: calculate_discount(x.price, 20),
    formatted: format_currency(calculate_discount(x.price, 20))
}
```

### Pattern Matching Examples

```jfm
// HTTP status code handling
match statusCode {
    200..299 => "Success",
    300..399 => "Redirect",
    400..499 => "Client Error",
    500..599 => "Server Error",
    _ => "Unknown"
}

// User role mapping
match user.role {
    "admin" => "Administrator",
    "user" => "Regular User",
    "guest" => "Guest Account",
    _ => "Unknown Role"
}
```

### Module Organization

```jfm
// utils.jfm
fn filter_active(items) {
    return items | .active == true;
}

fn format_date(timestamp) {
    // Formatting logic
}

// main.jfm
let utils = import("utils.jfm");
let activeItems = utils::filter_active(root.items);
```

## 🛠️ CLI Reference

### Command Syntax

```
jfm [OPTIONS] [JSON]
```

### Arguments

| Argument | Description |
|----------|-------------|
| `[JSON]` | Inline JSON string to process |

### Options

| Option | Short | Description |
|--------|-------|-------------|
| `--file <FILE>` | `-f` | JSON file path |
| `--query <QUERY>` | `-q` | Query string to execute |
| `--query-file <PATH>` | | Load query from file |
| `--out <FILE>` | `-o` | Output file path |
| `--limit <N>` | `-n` | Limit array results |
| `--stream` | | Stream mode for large files (NDJSON) |
| `--compact` | | Minified JSON output |
| `--color <WHEN>` | | Colors: `auto`, `always`, `never` [default: `auto`] |
| `--verbose` | `-v` | Debug mode |
| `--version` | `-V` | Show version |
| `--help` | `-h` | Show help |

### Examples

```bash
# Basic query
jfm -f data.json -q ".users | .name"

# Inline JSON
jfm '{"users": [{"name": "Alice"}]}' -q ".users[0].name"

# Save to file
jfm -f data.json -q ".users" -o output.json

# Stream NDJSON files
jfm --stream -q ".name" -f data.ndjson

# Interactive mode
jfm -f data.json

# Load query from file
jfm -f data.json --query-file query.jfm

# Compact output
jfm -f data.json -q ".users" --compact
```

### Shell Completions

Generate shell completions for better CLI experience:

```bash
jfm complete bash   # For bash
jfm complete zsh    # For zsh
jfm complete fish   # For fish
jfm complete powershell  # For PowerShell
```

## 📝 Language Notes

- **`root`** - Contains the input JSON data (implicitly available)
- **Return value** - Last expression in script is the return value
- **Comments** - Single-line comments with `//`
- **Array indices** - Zero-based indexing
- **Numbers** - 64-bit floating point (IEEE 754)
- **String comparison** - Case-sensitive
- **Variable scoping** - `let`/`const` are block-scoped, simple assignment uses function scope
- **Immutability** - `const` variables cannot be reassigned after declaration

## 🎨 VS Code Extension

Syntax highlighting is available for VS Code:

1. Copy `.vscode/extensions/jfm-language` to `~/.vscode/extensions/`
2. Restart VS Code
3. Open any `.jfm` file to see syntax highlighting

The extension provides:
- Syntax highlighting for all language constructs
- Keyword highlighting
- Built-in function recognition
- Template literal support
- Comment detection

## 🤝 Contributing

Contributions are welcome! Please feel free to submit issues, feature requests, or pull requests.

## 📄 License

This project is licensed under the MIT License.

---

**Made with ❤️ for JSON manipulation enthusiasts**
