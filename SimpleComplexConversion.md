
### (a ^b c)*
```
_1 : a b c _1      // Self-loop
   |               // Nothing
```
### (a ^b c . d ^e f)*
```
_1 : a b c _2      // Other-loop
   |               // Nothing
_2 : d e f _2      // Self-loop
   |               // Nothing
```
### (a ^b c)+
`(a ^b c . a ^b c)+`
```
_1 : a b c _2      // Other-loop
_2 : a b c _2      // Self-loop
   |               // Nothing
```
### (a ^b c . d ^e f)+
```
_1 : a b c _2      // Other-loop
_2 : d e f _2      // Self-loop
   |               // Nothing
```
