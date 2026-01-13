# jfm - JSON Query Language

`jfm` is a command-line tool for querying and transforming JSON data.

## Installation

```bash
cargo build --release
# Binary: target/release/jfm
```

## Quick Start

```bash
# Query a JSON file
jfm --file data.json --query "root.users | .age > 25"

# Inline JSON
jfm '{"users": [{"name": "Alice", "age": 30}]}' --query "root.users[0].name"

# Interactive mode (type query, exit with Ctrl+D)
jfm --file data.json

# Streaming large files
jfm --stream --limit 10 --query "root.name" --file huge.json
```

## CLI Options

```
jfm [OPTIONS] [JSON]

Arguments:
  [JSON]                     JSON string to process

Options:
  -f, --file <FILE>          JSON file path
  -q, --query <QUERY>        Query string
      --query-file <PATH>    Query file path
  -o, --out <FILE>           Output file
  -n, --limit <N>            Limit array results to N items
      --stream               Stream mode for large files
      --compact              Minified JSON output
      --color <WHEN>         Colors: auto|always|never [default: auto]
  -v, --verbose              Debug mode
  -V, --version              Version info
  -h, --help                 Help
```

Shell completions: `jfm complete bash|zsh|fish|powershell`

## Language Reference

### Data Types

```jfm
42, 3.14           // Numbers
"hello"            // Strings
`Hello ${name}!`   // Template literals
true, false        // Booleans
null               // Null
[1, 2, 3]          // Arrays
{"key": "value"}   // Objects
x => x * 2         // Lambdas
```

### Variables

```jfm
let x = 10;
x = 20;           // Reassign
```

### Field Access

```jfm
root.user.name             // Dot notation
root.users[0].name         // Array index
root?.missing?.field       // Optional chaining (returns null)
root.items.length          // Array length

// Short form (at top level, .field means root.field)
.users                     // Same as root.users
.users[0].name             // Same as root.users[0].name
```

### Operators

```jfm
// Arithmetic
+ - * / % ^

// Comparison
== != > < >= <=

// Logical
&& || !

// Ternary (conditional)
let status = age >= 18 ? "adult" : "minor";
let grade = score > 90 ? "A" : score > 80 ? "B" : "C";

// Match expression (like Rust)
let result = match status_code {
    200 => "OK",
    404 => "Not Found",
    500 => "Server Error",
    _ => "Unknown"
};
let label = match value {
    1 => "one",
    2 => "two", 
    _ => "many"
};

// Null coalescing
let name = user.nickname ?? user.name ?? "Anonymous";
let config = settings?.theme ?? "default";

// Range
1..5               // [1, 2, 3, 4, 5]

// Concatenation
"a" + "b"          // "ab"
[1, 2] + [3, 4]    // [1, 2, 3, 4]
```

### Control Flow

```jfm
// If-else
if age >= 18 { "adult" } else { "minor" }

// For loop
for user in root.users {
    print(user.name);
}

// While loop
while x < 10 { x = x + 1; }

// Break/continue
for x in 1..10 {
    if x == 5 { break; }
    if x % 2 == 0 { continue; }
}
```

### Functions

```jfm
// Lambda
let double = x => x * 2;
let add = (x, y) => x + y;

// Named function
fn factorial(n) {
    if n <= 1 { return 1; }
    return n * factorial(n - 1);
}
```

### Pipe Operator

The pipe operator `|` is the primary way to transform data. It automatically handles filtering, mapping, and transformation based on the expression type.

```jfm
// Filter: boolean expressions filter the array
.users | .age > 25               // Keep users where age > 25
.users | .active == true         // Keep active users

// Map: field access extracts values
.users | .name                   // Extract names → ["Alice", "Bob"]

// Transform: arithmetic on fields mutates and returns objects
.users | .age * 2                // Double everyone's age, return modified users
.users | .salary + 1000          // Give everyone a raise

// Lambda: use named parameters for complex transforms
.users | x => x.name             // Map with named param
.users | x => x.age > 25         // Filter with named param (returns bool)
.users | x => x.a + x.b          // Complex expression

// Chaining: combine operations
.users | .age > 25 | .name       // Filter adults, then get names
.users | .salary * 1.1 | .name   // Give raise, then get names
.items | x => x.price > 100 | x => x.name  // Filter expensive, get names

// Function calls: array is prepended as first argument
.numbers | sum()                 // Same as sum(.numbers)
.users | sort_by("age")          // Same as sort_by(.users, "age")
```

### Template Literals

String interpolation with backticks:

```jfm
// Basic interpolation
let greeting = `Hello, ${user.name}!`;
let info = `${user.name} is ${user.age} years old`;

// Expressions in interpolation
let summary = `Found ${count(items)} items totaling $${sum(items | .price)}`;
let result = `Status: ${active ? "ON" : "OFF"}`;

// Multi-line strings
let html = `
  <div>
    <h1>${title}</h1>
    <p>${content}</p>
  </div>
`;

// Escape sequences
`Line 1\nLine 2`           // Newline
`Tab:\tvalue`              // Tab
`Path: C:\\Users\\${name}` // Escaped backslash
`Cost: \$100`              // Literal dollar sign
`Say \`hello\``            // Escaped backtick
```

## Built-in Functions

### Array

| Function | Description | Example |
|----------|-------------|---------|
| `count(arr)` | Array length | `count([1,2,3])` → 3 |
| `sum(arr)` | Sum numbers | `sum([1,2,3])` → 6 |
| `avg(arr)` | Average | `avg([1,2,3])` → 2 |
| `min(arr)` | Minimum | `min([3,1,2])` → 1 |
| `max(arr)` | Maximum | `max([1,3,2])` → 3 |
| `take(arr, n)` | First n items | `take([1,2,3], 2)` → [1,2] |
| `unique(arr)` | Remove duplicates | `unique([1,1,2])` → [1,2] |
| `sort(arr)` | Sort primitives | `sort([3,1,2])` → [1,2,3] |
| `sort_by(arr, field)` | Sort by field | `sort_by(users, "age")` |
| `group_by(arr, field)` | Group by field | `group_by(users, "age")` |
| `reverse(arr)` | Reverse | `reverse([1,2,3])` → [3,2,1] |
| `slice(arr, start, end?)` | Slice array | `slice([1,2,3,4], 1, 3)` → [2,3] |
| `push(arr, val)` | Append | `push([1,2], 3)` → [1,2,3] |
| `pop(arr)` | Remove last | `pop([1,2,3])` → 3 |
| `shift(arr)` | Remove first | `shift([1,2,3])` → 1 |
| `flat(arr, depth?)` | Flatten | `flat([[1],[2]])` → [1,2] |
| `first(arr)` | First element | `first([1,2])` → 1 |
| `last(arr)` | Last element | `last([1,2])` → 2 |
| `zip(a, b)` | Pair arrays | `zip([1,2],[3,4])` → [[1,3],[2,4]] |
| `find(arr, fn)` | Find first match | `find([1,2,3], x => x > 1)` → 2 |
| `find_index(arr, fn)` | Find index | `find_index([1,2,3], x => x > 1)` → 1 |
| `every(arr, fn)` | All match? | `every([2,4], x => x % 2 == 0)` → true |
| `some(arr, fn)` | Any match? | `some([1,2], x => x > 1)` → true |
| `reduce(arr, fn, init)` | Reduce | `reduce([1,2,3], (a,v) => a+v, 0)` → 6 |
| `enumerate(arr)` | Index-value pairs | `enumerate(["a","b"])` → [[0,"a"],[1,"b"]] |
| `clone(arr)` | Deep copy | `clone([1,[2,3]])` → new array |

### String

| Function | Description | Example |
|----------|-------------|---------|
| `len(s)` | Length | `len("hello")` → 5 |
| `trim(s)` | Trim whitespace | `trim("  hi  ")` → "hi" |
| `upper(s)` | Uppercase | `upper("hi")` → "HI" |
| `lower(s)` | Lowercase | `lower("HI")` → "hi" |
| `split(s, delim)` | Split | `split("a,b", ",")` → ["a","b"] |
| `join(arr, delim)` | Join | `join(["a","b"], ",")` → "a,b" |
| `contains(s, sub)` | Contains? | `contains("hello", "ell")` → true |
| `starts_with(s, pre)` | Starts with? | `starts_with("hello", "he")` → true |
| `ends_with(s, suf)` | Ends with? | `ends_with("hello", "lo")` → true |
| `replace(s, from, to)` | Replace | `replace("hi", "i", "o")` → "ho" |

### Object

| Function | Description | Example |
|----------|-------------|---------|
| `keys(obj)` | Get keys | `keys({"a":1})` → ["a"] |
| `values(obj)` | Get values | `values({"a":1})` → [1] |
| `entries(obj)` | Key-value pairs | `entries({"a":1})` → [["a",1]] |
| `has(obj, key)` | Has key? | `has({"a":1}, "a")` → true |
| `merge(o1, o2)` | Merge objects | `merge({"a":1}, {"b":2})` |
| `deep_merge(o1, o2)` | Deep merge | `deep_merge({a:{x:1}}, {a:{y:2}})` |
| `set_path(obj, path, val)` | Set nested path | `set_path({}, "a.b", 1)` → {a:{b:1}} |
| `clone(obj)` | Deep copy | `clone({a:1})` → new object |

### Type

| Function | Description | Example |
|----------|-------------|---------|
| `typeof(v)` | Type name | `typeof(42)` → "number" |
| `is_null(v)` | Is null? | `is_null(null)` → true |
| `is_array(v)` | Is array? | `is_array([])` → true |
| `is_object(v)` | Is object? | `is_object({})` → true |
| `is_string(v)` | Is string? | `is_string("hi")` → true |
| `is_number(v)` | Is number? | `is_number(42)` → true |
| `is_bool(v)` | Is bool? | `is_bool(true)` → true |
| `to_string(v)` | Convert to string | `to_string(42)` → "42" |
| `to_number(v)` | Convert to number | `to_number("42")` → 42 |
| `to_int(v)` | Convert to integer (truncate) | `to_int(3.7)` → 3 |
| `to_float(v)` | Convert to float | `to_float(42)` → 42.0 |
| `to_bool(v)` | Convert to boolean | `to_bool(0)` → false |
| `parse_json(s)` | Parse JSON string | `parse_json("[1,2]")` → [1,2] |

### Math

| Function | Description |
|----------|-------------|
| `floor(n)` | Floor |
| `ceil(n)` | Ceiling |
| `round(n)` | Round |
| `abs(n)` | Absolute value |
| `sqrt(n)` | Square root |
| `pow(a, b)` | Power |
| `sin(n)`, `cos(n)`, `tan(n)` | Trigonometry |
| `random()` | Random 0-1 |

### I/O & Script

| Function | Description |
|----------|-------------|
| `print(args...)` | Print to stdout |
| `input(prompt?)` | Read line from stdin |
| `include(path)` | Execute external script |

### Generator Functions

| Function | Description | Example |
|----------|-------------|---------|
| `replicate(n, fn)` | Generate n items | `replicate(3, i => i * 10)` → [0,10,20] |
| `range(start, end, step?)` | Number sequence | `range(0, 10, 2)` → [0,2,4,6,8,10] |
| `cross(arr1, arr2, ...)` | Cartesian product | `cross([1,2], ["a","b"])` → [[1,"a"],[1,"b"],[2,"a"],[2,"b"]] |

### Spread Operator

Expand arrays and objects inline:

```jfm
// Array spread
let arr = [2, 3];
[1, ...arr, 4]             // [1, 2, 3, 4]

let a = [1, 2];
let b = [3, 4];
[...a, ...b]               // [1, 2, 3, 4]

// Object spread
let base = { a: 1, b: 2 };
{ ...base, c: 3 }          // { a: 1, b: 2, c: 3 }
{ ...base, a: 100 }        // { a: 100, b: 2 }  (override)

let x = { foo: 1 };
let y = { bar: 2 };
{ ...x, ...y, baz: 3 }     // { foo: 1, bar: 2, baz: 3 }
```

## Examples

### Filter and Transform

```jfm
// Filter adults and extract names
.users | .age >= 18 | .name

// Transform with mutation
.users | .salary * 1.1           // Give everyone 10% raise

// Get top 3 by score
take(sort_by(.users, "score"), 3)

// Group by department
group_by(.employees, "department")

// Chain with lambdas for complex logic
.users | x => x.age > 25 && x.active | x => x.name
```

### Aggregation

```jfm
let ages = .users | .age;
{
    "count": count(ages),
    "avg": avg(ages),
    "min": min(ages),
    "max": max(ages)
}
```

### Data Transformation

```jfm
// Transform each user into a new shape
.users | {
    "fullName": .firstName + " " + .lastName,
    "isAdult": .age >= 18,
    "status": .age > 30 ? "Senior" : "Junior"
}
```

### Working with Nested Data

```jfm
// Extract nested values
.company.employees | .department.name

// Optional chaining for missing fields
.user?.address?.city
```

### Using Loops

```jfm
let results = [];
for user in .users {
    if user.active {
        results = push(results, user.name);
    }
}
results

// Or use pipes instead (preferred)
.users | .active == true | .name
```

### Custom Functions

```jfm
fn is_adult(user) {
    return user.age >= 18;
}

fn format_name(user) {
    return user.firstName + " " + user.lastName;
}

// Use lambdas in pipes
.users | x => is_adult(x) | x => format_name(x)
```

### Modular Scripts

```jfm
// utils.jfm
fn process(data) { data | .active == true | .name }

// main query
let result = include("utils.jfm");
process(root.users)
```

### Using Generator Functions

```jfm
// Generate a sequence
range(1, 5)                                    // [1, 2, 3, 4, 5]
range(0, 100, 10)                              // [0, 10, 20, ..., 100]
range(10, 0, -2)                               // [10, 8, 6, 4, 2, 0]

// Create objects with replicate
replicate(5, i => { id: i, name: `Item ${i}` })
// [{id:0,name:"Item 0"}, {id:1,name:"Item 1"}, ...]

// Cartesian product
cross([1, 2], ["a", "b"])
// [[1,"a"], [1,"b"], [2,"a"], [2,"b"]]

// Deep merge objects
let defaults = { theme: "dark", size: { width: 100, height: 50 } };
let custom = { size: { width: 200 } };
deep_merge(defaults, custom)
// { theme: "dark", size: { width: 200, height: 50 } }

// Set nested paths
set_path({}, "config.db.host", "localhost")
// { config: { db: { host: "localhost" } } }

// Enumerate for index access
enumerate(["a", "b", "c"]) | map(pair => `${pair[0]}: ${pair[1]}`)
// ["0: a", "1: b", "2: c"]
```

### Building Complex Objects

```jfm
// Combine spread with generators
let base = { type: "user", active: true };
replicate(3, i => { ...base, id: i, name: `User ${i}` })
// [
//   { type: "user", active: true, id: 0, name: "User 0" },
//   { type: "user", active: true, id: 1, name: "User 1" },
//   { type: "user", active: true, id: 2, name: "User 2" }
// ]

// Clone to avoid mutation
let original = { data: [1, 2, 3] };
let copy = clone(original);
push(copy.data, 4);
// original.data is still [1, 2, 3]
// copy.data is [1, 2, 3, 4]
```

## Notes

- `root` contains the input JSON
- Array indices are zero-based
- Numbers are 64-bit floats
- String comparisons are case-sensitive
- Last expression is the return value
- Blocks create new variable scopes

