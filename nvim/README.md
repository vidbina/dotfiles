# Neovim Configuration

Populate the `~/.configs/nvim/coc-settings.json` file with a configuration in
line with the following configuration:

```json
{
  "languageserver": {
    "psalmls":{
      "command": "psalm --language-server",
      "filetypes": ["php"],
      "rootPatterns": ["psalm.xml", "psalm.xml.dist"],
      "requireRootPattern": true
    }
  }
}
```

Refer to the [CoC GitHub repo](https://github.com/neoclide/coc.nvim) for
further information.
