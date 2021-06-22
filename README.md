# cbexec
## Examples
### C
```c
#!/usr/bin/env cbexec

#include <stdio.h>

void main() {
    printf("Hello C World!\n");
}
```
```bash
 » ./hello.c
Hello C World!
```
---
### C++
```c++
#!/usr/bin/env cbexec

#include <iostream>

int main() {
    std::cout << "Hello C++ World!";
    return 0;
}
```
```bash
 » ./hello.cpp
Hello C++ World!
```
---
### D
```d
#!/usr/bin/env cbexec

import std.stdio;

int main() {
    writeln("Hello D World!");
    return 0;
}
```
```bash
 » ./hello.d
Hello D World!
```
---
### Go
```go
#!/usr/bin/env cbexec

package main

import "fmt"

func main() {
    fmt.Print("Hello Go World!\n")
}
```
```bash
 » ./hello.go
Hello Go World!
```
---
### FORTRAN-90
```fortran
#!/usr/bin/env cbexec

program main
implicit none
write (*, '(a)') 'Hello FORTRAN World!'
stop
end
```
```bash
 » ./hello.f
Hello FORTRAN-90 World!
```
---
### FORTRAN-77
```fortran
#!/usr/bin/env /home/jhj/src/official/personal/cbexec/cbexec

      PROGRAM HELLOW
      WRITE(UNIT=*,FMT='(A)')'HELLO FORTRAN-77 WORLD!'
      STOP
      END

```
```bash
 » ./hello.f77
HELLO FORTRAN-77 WORLD!
```
---
### COBOL
```cobol
#!/usr/bin/env cbexec

       IDENTIFICATION DIVISION.
       PROGRAM-ID. COBOL.
       PROCEDURE DIVISION.
       MAIN.
           DISPLAY 'HELLO COBOL WORLD!' END-DISPLAY.
           STOP RUN.
```
```bash
 » ./hello.cob
HELLO COBOL WORLD!
```
---
