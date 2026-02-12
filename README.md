# hoogle-mcp

An [MCP](https://modelcontextprotocol.io/) server that gives language models
access to [Hoogle](https://hoogle.haskell.org/), the Haskell API search engine.

## Tools

| Tool | Description |
|------|-------------|
| `hoogle_search` | Search Haskell libraries by function name, type signature, or qualified name. Supports package filters (`+base`, `-containers`) and configurable result count. |
| `hoogle_info` | Get detailed information about a specific function or type, including its signature, package, module, documentation, and Hackage link. |

## Building

Requires GHC >= 9.6 and Cabal >= 3.14.

```sh
cabal build
```

## Running tests

```sh
cabal test
```

## Configuration

### Claude Desktop

Add to your `claude_desktop_config.json`:

```json
{
  "mcpServers": {
    "hoogle": {
      "command": "cabal",
      "args": ["run", "hoogle-mcp"],
      "cwd": "/path/to/hoogle-mcp"
    }
  }
}
```

Or, if you have already built and installed the binary:

```json
{
  "mcpServers": {
    "hoogle": {
      "command": "hoogle-mcp"
    }
  }
}
```

### Claude Code

```sh
claude mcp add hoogle -- cabal run hoogle-mcp
```

### Environment variables

| Variable | Default | Description |
|----------|---------|-------------|
| `HOOGLE_URL` | `https://hoogle.haskell.org` | Base URL for the Hoogle API. Override this to point at a local Hoogle instance. |

## Protocol

The server communicates over **stdio** using [JSON-RPC 2.0](https://www.jsonrpc.org/specification),
implementing version `2024-11-05` of the Model Context Protocol.

## License

See the `license` field in `hoogle-mcp.cabal`.
