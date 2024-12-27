# Cardamom
Another programming language implementation (hopefully this time will be better).


## Example code
```cpp
import "std.io" as io;

public class Person(private name: string, private age: string) {
private:
    address: string = "1 High Road";

public:
    getAddress() -> string {
        return this.address;
    }

    moveHouse(address: Option<string>) -> void {
        this.address = address.value() ?? this.address; // either new address or old address
    }

    birthday() -> int {
        this.age += 1;

        return this.age;
    }

    getName() -> string {
        return this.name;
    }
}

fn main() -> void {
    let p: Person = new Person("Ethan", "17");

    io.println("Hello, $1! You live at $2.", p.getName(), p.getAddress());

    io.println("Happy $1 birthday, $2!", p.birthday(), p.getName());
}
```

## Progress
- [x] Lexing
- [x] Parsing
- [ ] Symbol Table
- [ ] Type Checking
- [ ] Code Generation
- [ ] Maybe GC?

I am currently developing this programming language as a hobby