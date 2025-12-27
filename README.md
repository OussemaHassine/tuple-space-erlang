# 🧩 Tuple Space in Erlang

An Erlang-based implementation of a **Tuple Space**: a coordination model inspired by the Linda paradigm. This project was developed for the _Programming Languages and Paradigms_ course at the University of Urbino.

---

## 📚 Introduction

A **Tuple Space** is a shared memory abstraction that allows concurrent processes to communicate by writing and reading tuples using **associative pattern matching**. It decouples producers and consumers in both time and space, making it well-suited for distributed and concurrent systems.

---

## ✅ Project Requirements

The system supports the following **interface** operations:

### 🔁 Core Operations

- `out(TS, Tuple)`  
  Insert a tuple into the space.

- `rd(TS, MatchFun)`  
  Blocking read (non-destructive). Waits for a match.

- `in(TS, MatchFun)`  
  Blocking read and remove. Waits for a match.

### ⏲️ Timeout Variants

- `rd(TS, MatchFun, Timeout)`  
- `in(TS, MatchFun, Timeout)`  

Return `{err, timeout}` if no match is found within the specified time.

### 🌐 Node Management

- `addNode(TS, Node)`  
- `removeNode(TS, Node)`  
- `nodes(TS)`  

Manage visibility of the tuple space across logical nodes.

### ✳️ Simplifications

- Flat tuples only  
- Pattern matching via Erlang functions (due to Erlang limitations)  
- No physical replication; node management is logical only  

---

## 🧠 Design Highlights

### 1. Server-Based Architecture
Implemented as a **single Erlang process** that holds:
- `tuples`: list of current tuples
- `nodes`: list of registered logical nodes

### 2. Blocking Operations
- If no match is found, the client **blocks** until one becomes available.
- Implemented without suspending the server, enabling continued responsiveness.

### 3. Timeout Support
Handled entirely on the client side via `receive ... after`.

### 4. Pattern Matching Strategy
Erlang doesn’t allow direct passing of patterns like `{10, _}` as arguments.
Instead, clients pass **matching functions**, e.g.:

```erlang
fun({10, _}) -> true; (_) -> false end.
