## clang-tidy

### CMake
To create `compile_commands.json`
```
cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON ..
```

### Auto fix
```
clang-tidy -fix -fix-errors
```
