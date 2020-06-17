[![Build Status](https://github.com/bobbygebert/verified/workflows/Rust/badge.svg)](https://github.com/bobbygebert/actions?workflow=Rust)

# Verifiable Rust

A collection of crates to facilitate the development of formally verifiable rust code.                                       

Type level programming allows us to implement logic that can be verified by the compiler, which
makes it possible to catch bugs at compile time, rather than at runtime.                       
                                                                                               
Say we have an algorithm where the runtime scales exponentially. We would like to be able to       
restrict the number of elements in our working set to a reasonable number, let's say 128, in   
order to ensure that the algorithm completes in a reasonable amount of time, every time.       
                                                                                               
```rust                                                                                
use verified::*;                                                                               
                                                                                               
#[derive(Default)]                                                                             
struct Collection<E, Size: Unsigned> {                                                         
    elements: Vec<E>,                                                                          
    size: Size,                                                                                
}                                                                                              
                                                                                               
#[verify]                                                                                      
fn slow_routine<E, Size: Unsigned>(working_set: Collection<E, Size>)                           
where                                                                                          
    // Restrict the size of the working set.                                                   
    _: Verify<{ Size < 128 }, { Size > 0 }>                                                    
{                                                                                              
    // TODO                                                                                    
}                                                                                              
                                                                                               
fn main() {                                                                                    
    // No problem here...                                                                      
    slow_routine::<String, U1>(Default::default());                                            
    slow_routine::<String, U127>(Default::default());                                          
                                                                                               
    // XXX: Does not compile because our working set is empty.                                 
    slow_routine::<String, U0>(Default::default());                                            
                                                                                               
    // XXX: Does not compile because our working set is one element too large.                 
    slow_routine::<String, U128>(Default::default());                                          
}                                                                                              
```                                                                                            
                                                                                               
For a more complete example, see the `vec` module. Here is an abbreviated snippet:             
                                                                                               
```rust                                                                                            
use verified::*;                                                                               
use std::vec::Vec as Raw;                                                                      
                                                                                               
pub struct Vec<Size: Unsigned, Element>(Size, Raw<Element>);                                   
                                                                                               
#[verify]                                                                                      
impl<Size: Unsigned, Element> Vec<Size, Element> {                                             
    pub fn append<OtherSize: Unsigned>(                                                        
        self,                                                                                  
        other: Vec<OtherSize, Element>,                                                        
    ) -> Vec<{ Size + OtherSize }, Element> {                                                  
        self + other                                                                           
    }                                                                                          
                                                                                               
    pub fn pop(self) -> (Vec<{ Size - 1 }, Element>, Element)                                  
    where                                                                                      
        _: Verify<{ Size > 0 }>,                                                               
    {                                                                                          
        self.into()                                                                            
    }                                                                                          
                                                                                               
    pub fn push(self, e: Element) -> Vec<{ Size + 1 }, Element> {                              
        (self, e).into()                                                                       
    }                                                                                          
}                                                                                              
                                                                                               
#[verify]                                                                                      
impl<SizeL: Unsigned, SizeR: Unsigned, Element> std::ops::Add<Vec<SizeR, Element>>             
    for Vec<SizeL, Element>                                                                    
{                                                                                              
    type Output = Vec<{ SizeL + SizeR }, Element>;                                             
    fn add(self, Vec(os, mut ov): Vec<SizeR, Element>) -> Self::Output {                       
        let Self(s, mut v) = self;                                                             
        v.append(&mut ov);                                                                     
        Vec(s + os, v)                                                                         
    }                                                                                          
}                                                                                              
                                                                                               
#[verify]                                                                                      
impl<Size: Unsigned, Element> std::convert::From<(Vec<Size, Element>, Element)>                
    for Vec<{ Size + 1 }, Element>                                                             
{                                                                                              
    fn from((Vec(_, mut v), e): (Vec<Size, Element>, Element)) -> Self {                       
        v.push(e);                                                                             
        Self(Default::default(), v)                                                            
    }                                                                                          
}                                                                                              
                                                                                               
#[verify]                                                                                      
impl<Size: Unsigned, Element> std::convert::From<Vec<Size, Element>>                           
    for (Vec<{ Size - 1 }, Element>, Element)                                                  
where                                                                                          
    _: Verify<{ Size > 0 }>,                                                                   
{                                                                                              
    fn from(Vec(_, mut v): Vec<Size, Element>) -> Self {                                       
        let e = v.pop().unwrap();                                                              
        (Vec(Default::default(), v), e)                                                        
    }                                                                                          
}                                                                                              
```                                                                                            
# $ cargo-verify

The `verified` crate is built on top of the `typenum` crate. Naturally, the compiler errors can get pretty hairy. Here, I've accidentally typed `2` instead of `1` somewhere in the `vec` module. This is perhaps one of the less cryptic errors you may see...

```
$ cargo build

error[E0277]: cannot add `typenum::uint::UInt<typenum::uint::UTerm, typenum::bit::B1>` to `Size`
  --> verified/src/vec.rs:44:19
   |
44 |         (self, e).into()
   |                   ^^^^ no implementation for `Size + typenum::uint::UInt<typenum::uint::UTerm, typenum::bit::B1>`
   |
   = help: the trait `std::ops::Add<typenum::uint::UInt<typenum::uint::UTerm, typenum::bit::B1>>` is not implemented for `Size`
help: consider further restricting this bound with `+ std::ops::Add<typenum::uint::UInt<typenum::uint::UTerm, typenum::bit::B1>>`
  --> verified/src/vec.rs:28:12
   |
28 | impl<Size: Unsigned, Element> Vec<Size, Element> {
   |            ^^^^^^^^
   = note: required because of the requirements on the impl of `std::convert::From<(vec::Vec<Size, Element>, Element)>` for `vec::Vec<<Size as std::ops::Add<typenum::uint::UInt<typenum::uint::UInt<typenum::uint::UTerm, typenum::bit::B1>, typenum::bit::B0>>>::Output, Element>`
   = note: required because of the requirements on the impl of `std::convert::Into<vec::Vec<<Size as std::ops::Add<typenum::uint::UInt<typenum::uint::UInt<typenum::uint::UTerm, typenum::bit::B1>, typenum::bit::B0>>>::Output, Element>>` for `(vec::Vec<Size, Element>, Element)`
```

`cargo-verify` tries to help by translating types into simple arithmetic expressions where possible.

```
$ cargo verify build

error[E0277]: cannot add `1` to `Size`
  --> verified/src/vec.rs:44:19
   |
44 |         (self, e).into()
   |                   ^^^^ no implementation for `Size + 1`
   |
   = help: the trait `{ _ + 1 }` is not implemented for `Size`
help: consider further restricting this bound with `+ { _ + 1 }`
  --> verified/src/vec.rs:28:12
   |
28 | impl<Size: Unsigned, Element> Vec<Size, Element> {
   |            ^^^^^^^^
   = note: required because of the requirements on the impl of `std::convert::From<(vec::Vec<Size, Element>, Element)>` for `vec::Vec<{ Size + 2 }, Element>`
   = note: required because of the requirements on the impl of `std::convert::Into<vec::Vec<{ Size + 2 }, Element>>` for `(vec::Vec<Size, Element>, Element)`


```

## Install
```
$ cargo install cargo-verify
```

To upgrade:

```
$ cargo install --force cargo-verify
```

Or clone and build with `$ cargo build` then place the binary in your $PATH.

## Usage

```
$ cargo verify [COMMAND] [OPTIONS]...
```
