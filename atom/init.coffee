fs = require 'fs'

fs.stat process.env.HOME + '/.opam/system/bin', (err, stats) ->
  if stats? and stats.isDirectory()
    # Add opam bin directory to PATH.
    process.env.PATH = [process.env.HOME + '/.opam/system/bin', process.env.PATH].join(':')
  else
    console.warn('Opam system bin does not exist.')

atom.packages.onDidActivatePackage (pack) ->
  if pack.name == 'platformio-ide-terminal'
    atom.commands.add 'atom-workspace',
      'editor:focus-toggle', ->
        activePane = atom.workspace.getActivePane()
        panels = atom.workspace.getBottomPanels()
        terminal = panels.find (panel) ->
          panel.item.constructor.name == 'PlatformIOTerminalView' and panel.visible
        if not terminal
          editor = atom.workspace.getActiveTextEditor()
          atom.commands.dispatch(atom.views.getView(editor), 'platformio-ide-terminal:new')
        else if terminal and activePane.focused isnt false
          terminal.item.focus()
        else if terminal and activePane.focused is false
          terminal.item.blur()
          activePane.activate()
