local which = require('which-key')

which.setup()

which.register({
    f = {
          name = 'Find',
          b = { ':Telescope buffers<cr>', 'Buffers' },
          f = { ':Telescope find_files<cr>', 'File' },
          g = { ':Telescope live_grep<cr>', 'Grep' }
    },
    h = {
          name = 'Hop',
          c = { ':HopChar1<cr>', 'Character Hop' },
          b = { ':HopChar2<cr>', 'Bigram Hop' },
          w = { ':HopWord<cr>', 'Word Hop' },
          p = { ':HopPattern<cr>', 'Pattern Hop' },
          l = { ':HopLine<cr>', 'Line Hop' },
          s = { ':HopLineStart<cr>', 'Start Line Hop' }
    },
  },
  { prefix = '<leader>' })
