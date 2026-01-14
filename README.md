# jfm

A powerful command-line tool for querying and transforming JSON data.

## Installation

```bash
cargo build --release
# Binary: target/release/jfm
```

## Quick Start

```bash
# Query a JSON file
jfm -f data.json -q ".users | .age > 25"

# Inline JSON
jfm '{"name": "Alice", "age": 30}' -q ".name"

# Interactive mode
jfm -f data.json

# Stream large files
jfm --stream --limit 10 -q ".name" -f huge.json
```

## CLI Options

```
jfm [OPTIONS] [JSON]

Arguments:
  [JSON]                     Inline JSON string to process

Options:
  -f, --file <FILE>          JSON file path
  -q, --query <QUERY>        Query string
      --query-file <PATH>    Load query from file
  -o, --out <FILE>           Output file
  -n, --limit <N>            Limit array results
      --stream               Stream mode for large files
      --compact              Minified JSON output
      --color <WHEN>         Colors: auto|always|never [default: auto]
  -v, --verbose              Debug mode
  -V, --version              Show version
  -h, --help                 Show help
```

Generate shell completions:

```bash
jfm complete bash|zsh|fish|powershell
```

---

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
x = 10             // Simple assignment (let is optional)
let y = 20         // let is also supported
x = 30             // Reassign
```

### Field Access

```jfm
root.user.name           // Dot notation
root.users[0].name       // Array index
root?.missing?.field     // Optional chaining (returns null if missing)
.users                   // Short form (same as root.users)
[1, 2, 3].length         // Array length property
```

### Operators

```jfm
// Arithmetic
+ - * / % ^

// Comparison
== != > < >= <=

// Logical
&& || !

// Ternary
age >= 18 ? "adult" : "minor"

// Match expression
match status {
    200 => "OK",
    404 => "Not Found",
    _ => "Unknown"
}

// Null coalescing
user.nickname ?? user.name ?? "Anonymous"

// Range
1..5               // [1, 2, 3, 4, 5]

// String/Array concatenation
"a" + "b"          // "ab"
[1, 2] + [3, 4]    // [1, 2, 3, 4]

// Spread operator
[1, ...arr, 4]     // Expand array
{ ...base, c: 3 }  // Expand object

// Shorthand properties
{name, age}        // Same as {name: name, age: age}

// Replication
3 * { id: @ }      // [{ id: 0 }, { id: 1 }, { id: 2 }]
```

### Control Flow

```jfm
// If-else
if age >= 18 { "adult" } else { "minor" }

// For loop
for user in users { print(user.name); }

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
double = x => x * 2
add = (a, b) => a + b

// Short function syntax
multiply(a, b) => a * b

// Named function with block
fn factorial(n) {
    if n <= 1 { return 1; }
    return n * factorial(n - 1);
}
```

---

## Pipe Operator

The `|` operator is the primary way to transform data. It handles filtering, mapping, and transformation based on the expression type.

### Filter (boolean expression)

```jfm
.users | .age > 25           // Keep users where age > 25
.users | .active == true     // Keep active users
```

### Map (field access)

```jfm
.users | .name               // Extract names → ["Alice", "Bob"]
```

### Transform (arithmetic on fields)

```jfm
.users | .age * 2            // Double everyone's age, return modified users
.users | .salary + 1000      // Give everyone a raise
```

### Nested field mutations

```jfm
.users | .profile.age + 5         // Add 5 to nested profile.age
.users | .profile.name = "New"    // Set nested field value
.users | .items[0] = "first"      // Set array element
```

### Lambda expressions

```jfm
.users | x => x.name              // Map with named parameter
.users | x => x.age > 25          // Filter with named parameter
.users | x => x.a + x.b           // Complex expression
```

### The @ variable

Use `@` to explicitly reference the current item in a pipe:

```jfm
.numbers | @ > 5                  // Filter numbers > 5
.numbers | @ * 2                  // Double each number
.users | @.age > 25 && @.active   // Complex filter
```

### Named pipe context with `as`

Name the current item for clearer code:

```jfm
.users | @ as user | user.name   // Name current item "user"
.cells | @ as cell | 3 * {       // Combine with replication
    ...user_template,
    cell_id: cell.id,
    index: @                      // @ is now replication index
}
```

### Chaining

```jfm
.users | .age > 25 | .name        // Filter adults, then get names
.items | .price > 100 | .name     // Filter expensive, get names
```

### Indexing with [n]

```jfm
.users | .active == true | [0]    // Get first active user
.items | .price > 100 | [0]       // Get first expensive item
```

### Function calls in pipes

Functions work seamlessly in pipes:

```jfm
.numbers | sum                    // Bare function name: sum(.numbers)
.numbers | sum()                  // Also works: sum(.numbers)
.users | sort_by("age")           // With args: sort_by(.users, "age")
```

### Method-style calls

Call functions as methods:

```jfm
.numbers.sum()                    // Same as sum(.numbers)
.users.sort_by("age")             // Same as sort_by(.users, "age")
str.upper()                       // Same as upper(str)
cell.get_cell_id()                // Call user-defined function
```

---

## Template Literals

String interpolation with backticks:

```jfm
let greeting = `Hello, ${user.name}!`;
let summary = `Found ${items.length} items totaling $${sum(items | .price)}`;

// Multi-line strings
let html = `
  <div>
    <h1>${title}</h1>
  </div>
`;

// Escape sequences
`Line 1\nLine 2`           // Newline
`Tab:\tvalue`              // Tab
`Path: C:\\Users`          // Backslash
`Cost: \$100`              // Literal $
`Say \`hello\``            // Backtick
```

---

## Built-in Functions

### Array Functions

| Function | Description | Example |
|----------|-------------|---------|
| `arr.length` | Array length | `[1,2,3].length` → `3` |
| `sum(arr)` | Sum numbers | `sum([1,2,3])` → `6` |
| `avg(arr)` | Average | `avg([1,2,3])` → `2` |
| `min(arr)` | Minimum | `min([3,1,2])` → `1` |
| `max(arr)` | Maximum | `max([1,3,2])` → `3` |
| `unique(arr)` | Remove duplicates | `unique([1,1,2])` → `[1,2]` |
| `sort(arr)` | Sort primitives | `sort([3,1,2])` → `[1,2,3]` |
| `sort_by(arr, field)` | Sort by field | `sort_by(users, "age")` |
| `group_by(arr, field)` | Group by field | `group_by(users, "dept")` |
| `reverse(arr)` | Reverse | `reverse([1,2,3])` → `[3,2,1]` |
| `slice(arr, start, end?)` | Slice array | `slice([1,2,3,4], 1, 3)` → `[2,3]` |
| `push(arr, val)` | Append | `push([1,2], 3)` → `[1,2,3]` |
| `pop(arr)` | Remove last | `pop([1,2,3])` → `3` |
| `shift(arr)` | Remove first | `shift([1,2,3])` → `1` |
| `flat(arr, depth?)` | Flatten | `flat([[1],[2]])` → `[1,2]` |
| `first(arr)` | First element | `first([1,2])` → `1` |
| `last(arr)` | Last element | `last([1,2])` → `2` |
| `zip(a, b)` | Pair arrays | `zip([1,2],[3,4])` → `[[1,3],[2,4]]` |
| `find(arr, fn)` | Find first match | `find([1,2,3], x => x > 1)` → `2` |
| `find_index(arr, fn)` | Find index | `find_index([1,2,3], x => x > 1)` → `1` |
| `every(arr, fn)` | All match? | `every([2,4], x => x % 2 == 0)` → `true` |
| `some(arr, fn)` | Any match? | `some([1,2], x => x > 1)` → `true` |
| `reduce(arr, fn, init)` | Reduce | `reduce([1,2,3], (a,v) => a+v, 0)` → `6` |
| `enumerate(arr)` | Index-value pairs | `enumerate(["a","b"])` → `[[0,"a"],[1,"b"]]` |
| `flat_map(arr, fn)` | Map then flatten | `flat_map([1,2], x => [x, x*10])` → `[1,10,2,20]` |
| `clone(val)` | Deep copy | `clone([1,[2,3]])` |

### String Functions

| Function | Description | Example |
|----------|-------------|---------|
| `len(s)` | Length | `len("hello")` → `5` |
| `trim(s)` | Trim whitespace | `trim("  hi  ")` → `"hi"` |
| `upper(s)` | Uppercase | `upper("hi")` → `"HI"` |
| `lower(s)` | Lowercase | `lower("HI")` → `"hi"` |
| `split(s, delim)` | Split | `split("a,b", ",")` → `["a","b"]` |
| `join(arr, delim)` | Join | `join(["a","b"], ",")` → `"a,b"` |
| `contains(s, sub)` | Contains? | `contains("hello", "ell")` → `true` |
| `starts_with(s, pre)` | Starts with? | `starts_with("hello", "he")` → `true` |
| `ends_with(s, suf)` | Ends with? | `ends_with("hello", "lo")` → `true` |
| `replace(s, from, to)` | Replace | `replace("hi", "i", "o")` → `"ho"` |

### Object Functions

| Function | Description | Example |
|----------|-------------|---------|
| `keys(obj)` | Get keys | `keys({"a":1})` → `["a"]` |
| `values(obj)` | Get values | `values({"a":1})` → `[1]` |
| `entries(obj)` | Key-value pairs | `entries({"a":1})` → `[["a",1]]` |
| `has(obj, key)` | Has key? | `has({"a":1}, "a")` → `true` |
| `merge(o1, o2)` | Merge objects | `merge({"a":1}, {"b":2})` |
| `deep_merge(o1, o2)` | Deep merge | `deep_merge({a:{x:1}}, {a:{y:2}})` |
| `set_path(obj, path, val)` | Set nested path | `set_path({}, "a.b", 1)` → `{a:{b:1}}` |
| `clone(obj)` | Deep copy | `clone({a:1})` |

### Type Functions

| Function | Description | Example |
|----------|-------------|---------|
| `typeof(v)` | Type name | `typeof(42)` → `"number"` |
| `is_null(v)` | Is null? | `is_null(null)` → `true` |
| `is_array(v)` | Is array? | `is_array([])` → `true` |
| `is_object(v)` | Is object? | `is_object({})` → `true` |
| `is_string(v)` | Is string? | `is_string("hi")` → `true` |
| `is_number(v)` | Is number? | `is_number(42)` → `true` |
| `is_bool(v)` | Is bool? | `is_bool(true)` → `true` |
| `to_string(v)` | To string | `to_string(42)` → `"42"` |
| `to_number(v)` | To number | `to_number("42")` → `42` |
| `to_int(v)` | To integer | `to_int(3.7)` → `3` |
| `to_float(v)` | To float | `to_float(42)` → `42.0` |
| `to_bool(v)` | To boolean | `to_bool(0)` → `false` |
| `parse_json(s)` | Parse JSON | `parse_json("[1,2]")` → `[1,2]` |

### Math Functions

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

### Generator Functions

| Function | Description | Example |
|----------|-------------|---------|
| `replicate(n, fn)` | Generate n items | `replicate(3, i => i * 10)` → `[0,10,20]` |
| `range(start, end, step?)` | Number sequence | `range(0, 10, 2)` → `[0,2,4,6,8,10]` |
| `cross(arr1, arr2, ...)` | Cartesian product | `cross([1,2], ["a","b"])` → `[[1,"a"],[1,"b"],[2,"a"],[2,"b"]]` |

### I/O Functions

| Function | Description |
|----------|-------------|
| `print(args...)` | Print to stdout |
| `input(prompt?)` | Read line from stdin |
| `include(path)` | Execute external script |

---

## Examples

### Filter and Transform

```jfm
// Get names of adults
.users | .age >= 18 | .name

// Give everyone a 10% raise
.users | .salary * 1.1

// Get top 3 by score
slice(sort_by(.users, "score"), 0, 3)

// Group by department
group_by(.employees, "department")
```

### Aggregation

```jfm
let ages = .users | .age;
{
    "count": ages.length,
    "avg": avg(ages),
    "min": min(ages),
    "max": max(ages)
}
```

### Data Transformation

```jfm
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

// Optional chaining
.user?.address?.city ?? "Unknown"
```

### Custom Functions

```jfm
fn is_adult(user) {
    return user.age >= 18;
}

fn format_name(user) {
    return `${user.firstName} ${user.lastName}`;
}

.users | x => is_adult(x) | x => format_name(x)
```

### Generator Examples

```jfm
// Generate sequence
range(1, 5)                    // [1, 2, 3, 4, 5]
range(0, 100, 10)              // [0, 10, 20, ..., 100]

// Replication with *
3 * { id: @ }                  // [{ id: 0 }, { id: 1 }, { id: 2 }]
3 * { id: @, name: `Item ${@}` }  // @ is the index

// Create objects with replicate function
replicate(5, i => { id: i, name: `Item ${i}` })

// Cartesian product
cross([1, 2], ["a", "b"])      // [[1,"a"], [1,"b"], [2,"a"], [2,"b"]]

// Deep merge
let defaults = { theme: "dark", size: { width: 100 } };
let custom = { size: { width: 200 } };
deep_merge(defaults, custom)   // { theme: "dark", size: { width: 200 } }

// Set nested paths
set_path({}, "config.db.host", "localhost")
// { config: { db: { host: "localhost" } } }
```

### Spread Operator

```jfm
// Array spread
let arr = [2, 3];
[1, ...arr, 4]                 // [1, 2, 3, 4]

// Object spread
let base = { a: 1, b: 2 };
{ ...base, c: 3 }              // { a: 1, b: 2, c: 3 }
{ ...base, a: 100 }            // { a: 100, b: 2 } (override)

// Nested path keys in object literals
let cell = { name: "cell1", config: { x: 1 } };
{ ...cell, config.y: 2 }       // { name: "cell1", config: { x: 1, y: 2 } }

// Works great with replicate
let template = { type: "item", data: { active: true } };
replicate(3, i => { ...template, data.id: i })
// Creates 3 items with data.id set to 0, 1, 2
```

### Modular Scripts

```jfm
// utils.jfm
fn process(data) { data | .active == true | .name }

// main.jfm
let result = include("utils.jfm");
process(root.users)
```

---

## Notes

- `root` contains the input JSON data
- If no explicit return, `root` is returned by default
- Comments: `// single line comments`
- Array indices are zero-based
- Numbers are 64-bit floats
- String comparisons are case-sensitive
- Blocks `{ }` create new variable scopes

## VS Code Extension

Syntax highlighting is available in `.vscode/extensions/jfm-language`. To install:

1. Copy the folder to `~/.vscode/extensions/`
2. Restart VS Code
3. Open any `.jfm` file
