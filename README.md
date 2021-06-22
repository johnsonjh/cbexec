# cbexec

## Examples

### Bash
```bash
#!/usr/bin/env cbexec

builtin printf '%s\n' "Hello Bash World!"
```

### C
```c
#!/usr/bin/env cbexec

#include <stdio.h>

int main() {
    printf("Hello C World!\n");
	return 0;
}
```

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

### C++
```c++
#!/usr/bin/env cbexec

#include <iostream>

int main()
{
   std::cout << "Hello C++ World!" << std::endl;
   return 0;
}
```

### C Shell
```csh
#!/usr/bin/env cbexec

set fignore = (.o \~) && set hw='Hello C Shell World!' && printf '%s\n' "$hw";
```

### D
```d
#!/usr/bin/env cbexec

import std.stdio;

int main() {
    writeln("Hello D World!");
	return 0;
}
```

### Emacs Lisp
```elisp
#!/usr/bin/env cbexec

(message "Hello Emacs Lisp World!")
```

### FORTRAN-90
```fortran
#!/usr/bin/env cbexec

program main
implicit none
write (*, '(a)') 'Hello FORTRAN-90 World!'
stop
end
```

### FORTRAN-77
```fortran
#!/usr/bin/env cbexec

      PROGRAM HELLOW
      WRITE(UNIT=*,FMT='(A)')'HELLO FORTRAN-77 WORLD!'
      STOP
      END
```

### Go
```go
#!/usr/bin/env cbexec

package main

import "fmt"

func main() {
	fmt.Println("Hello Go World!")
}
```

# Korn Shell
```ksh
#!/usr/bin/env cbexec

PATH=""
print "Hello Korn Shell World!"
```

### MATLAB / Octave
```matlab
#!/usr/bin/env cbexec

printf("Hello MATLAB World!\n");
quit(0);
```

### OCaml
```ocaml
#!/usr/bin/env cbexec

print_sring "Hello OCaml World!\n"
```

### POSIX Shell
```sh
#!/usr/bin/env cbexec

(
	exec 2> /dev/null
	time 1> /dev/null 2>&1 < /dev/null
) || {
	XX="Hello POSIX Shell World!" :
	printf '%s\n' "$XX"
} | grep POSIX
```

### Z Shell
```zsh
#!/usr/bin/env cbexec

PATH=""
- builtin print "Hello Z Shell World!"
```
