# Hello Module Example

This example shows a minimal Kansei module layout that can be installed with
`kansei install`.

Layout:
```
examples/hello_module/
  kansei.toml
  modules/
    hello_module.ks
  demo.ks
```

Install locally:
```
kansei install examples/hello_module
```

Run the demo from the example directory:
```
cd examples/hello_module
kansei demo.ks
```
