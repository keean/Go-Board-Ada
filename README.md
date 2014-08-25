Go-Board-Ada
============

Very fast Go board implemented in Ada, with Monte-Carlo random play outs. Ported from C++ generic concepts version, to use Ada generics with the aim of matching the C++ performance. By using some pointer arithmetic inside the critical low-level containers it gets the same performance as the C++ version. This more or less confirms that when using the same backend (GCC/GNAT) and the same algorithms performance is equivalent. Ada perhaps makes this slightly easier by removing the need to worry about references and move semantics etc.  
