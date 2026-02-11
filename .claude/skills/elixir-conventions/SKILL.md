---
name: elixir-conventions
description: Elixir-specific coding patterns and conventions.
---

# Elixir Conventions

## Control Flow

- Prefer `with` chains over nested `case`.
- Use `Enum.reduce_while` or `Enum.flat_map` over `Enum.reduce`
  with internal conditionals.
- Pattern match in function heads rather than branching in the body.

## Types & Structs

- Use `typedstruct` for struct definitions.
- Startup arguments use a `startup_options` union type:
  ```elixir
  @type startup_options ::
          {:node_id, String.t()}
          | {:id, atom()}

  @spec start_link(list(startup_options())) :: GenServer.on_start()
  ```
- Keep type definitions near the top, after `use`/`alias` blocks.

## GenServer / CQRS

- `call` for reads or synchronization only. Writes use `cast`.
- `handle_call` body: `{:reply, do_foo(...), state}` — core logic
  in the `do_` function.
- Never call a callback from another callback.
- Actor modules require three sections:
  ```elixir
  ############################################################
  #                      Public RPC API                      #
  ############################################################

  ############################################################
  #                    GenServer Behavior                    #
  ############################################################

  ############################################################
  #                  GenServer Implementation                #
  ############################################################
  ```

## Documentation

- First-person voice: "I am the X module." / "I return the Y."
- `@moduledoc` starts with one-sentence purpose.
- `### Public API` section listing all public functions.
- All public functions need `@doc` and `@spec`.

## Examples

- Live in `lib/examples/e_<module>.ex`.
- Module name pattern: `E<Module>` (e.g., `ENode`, `EShard`).
- Use `import ExUnit.Assertions` for `assert`.

## Interactive Testing

- Run one-off expressions: `timeout 60 mix run -e 'code'`
  (never use `--no-halt`, it hangs the VM).
- Inspect process state: `:sys.get_state(pid)`
- Query Mnesia:
  ```elixir
  :mnesia.transaction(fn -> :mnesia.match_object({table, :_, :_}) end)
  ```

## Formatting

- 78 character line length.
- Run `mix format` before finalizing.
- Section headers use banner comments:
  ```elixir
  ############################################################
  #                      Section Name                        #
  ############################################################
  ```
