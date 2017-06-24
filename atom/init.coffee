fs = require 'fs'

fs.stat process.env.HOME + '/.opam/system/bin', (err, stats) ->
  if stats.isDirectory()
    # Add opam bin directory to PATH.
    process.env.PATH = [process.env.HOME + '/.opam/system/bin', process.env.PATH].join(':')
  else
    console.warn('Opam system bin does not exist.')
