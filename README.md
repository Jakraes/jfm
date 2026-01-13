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

// Null coalescing
let name = user.nickname ?? user.name ?? "Anonymous";
let config = settings?.theme ?? "default";

// Compound assignment
+= -= *= /=

// Range
1..5               // [1, 2, 3, 4, 5]

// Spread operator
let base = { a: 1, b: 2 };
let extended = { ...base, c: 3 };  // { a: 1, b: 2, c: 3 }

let arr1 = [1, 2];
let arr2 = [0, ...arr1, 3];        // [0, 1, 2, 3]

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

Filter and transform arrays:

```jfm
root.users | .age > 25           // Filter: users over 25
root.users | .name               // Map: extract names
root.users | .age > 25 | .name   // Chain: filter then map
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
| `replicate(n, fn)` | Generate n items | `replicate(3, i => i*10)` → [0,10,20] |
| `range(start, end, step?)` | Number sequence | `range(0, 10, 2)` → [0,2,4,6,8,10] |
| `cross(arr1, arr2, ...)` | Cartesian product | `cross([1,2], ["a","b"])` → [[1,"a"],[1,"b"],[2,"a"],[2,"b"]] |
| `clone(v)` | Deep clone | `clone(obj)` → new copy |

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
| `merge(o1, o2)` | Shallow merge | `merge({"a":1}, {"b":2})` |
| `deep_merge(o1, o2)` | Deep recursive merge | `deep_merge({a:{x:1}}, {a:{y:2}})` → {a:{x:1,y:2}} |
| `set_path(obj, path, val)` | Set nested path | `set_path({}, "a.b.c", 1)` → {a:{b:{c:1}}} |

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

## Examples

### Filter and Transform

```jfm
// Filter adults and extract names
root.users | .age >= 18 | .name

// Get top 3 by score
take(sort_by(root.users, "score"), 3)

// Group by department
group_by(root.employees, "department")
```

### Aggregation

```jfm
let ages = root.users | .age;
{
    "count": count(ages),
    "avg": avg(ages),
    "min": min(ages),
    "max": max(ages)
}
```

### Data Transformation

```jfm
root.users | {
    "fullName": .firstName + " " + .lastName,
    "isAdult": .age >= 18,
    "status": if .age > 30 { "Senior" } else { "Junior" }
}
```

### Working with Nested Data

```jfm
// Extract nested values
root.company.employees | .department.name

// Optional chaining for missing fields
root.user?.address?.city
```

### Using Loops

```jfm
let results = [];
for user in root.users {
    if user.active {
        results = push(results, user.name);
    }
}
results
```

### Custom Functions

```jfm
fn is_adult(user) {
    return user.age >= 18;
}

fn format_name(user) {
    return user.firstName + " " + user.lastName;
}

root.users | is_adult(.) | format_name(.)
```

### Modular Scripts

```jfm
// utils.jfm
fn process(data) { data | .active == true | .name }

// main query
let result = include("utils.jfm");
process(root.users)
```

### Test Data Generation

Generate test data with templates and combinations:

```jfm
// Base templates
let base_user = { role: "customer", active: true };
let base_product = { category: "electronics", in_stock: true };

// Generate 100 users with spread operator
let users = replicate(100, i => ({
    ...base_user,
    id: i,
    loyalty_points: 1000 + i
}));

// Generate test matrix with cross product
let test_cases = cross(
    ["small", "medium", "large"],  // sizes
    [9.99, 19.99, 29.99],            // price points
    ["red", "blue", "green"]        // colors
);

// Create products with deep merge for config overlays
let products = replicate(4, i => 
    deep_merge(base_product, { 
        details: { sku: i, shipping: { weight: 100 + i * 10 } }
    })
);

// Set nested paths
let config = set_path({}, "store.warehouse.capacity", 500);

// Iterate with index using enumerate
for pair in enumerate(users) {
    let idx = pair[0];
    let user = pair[1];
    user.tier = idx % 3;
}

{ products: products, users: users, test_cases: test_cases };
```

## Notes

- `root` contains the input JSON
- Array indices are zero-based
- Numbers are 64-bit floats
- String comparisons are case-sensitive
- Last expression is the return value
- Blocks create new variable scopes

