return {
  'goolord/alpha-nvim',
  config = function()
    local alpha = require('alpha')
    local dashboard = require('alpha.themes.dashboard')
    local lazy = require('lazy')

    local function button(sc, txt, kb, kb_opts)
      local dashboard_button = dashboard.button(sc, txt, kb, kb_opts)
      dashboard_button.opts.hl = 'Function'
      dashboard_button.opts.hl_shortcut = 'Type'
      return dashboard_button
    end

    local function footer()
      -- local plugin_count = #vim.tbl_keys(packer_plugins)
      local datetime = os.date('  %d-%m-%Y   %H:%M:%S')
      return datetime
          .. '  '
          .. #lazy.plugins()
          .. '   v'
          .. vim.version().major
          .. '.'
          .. vim.version().minor
          .. '.'
          .. vim.version().patch
    end

    dashboard.section.header.val = {
      [[                 ......:::::::::::::----     ]],
      [[                .=====================-      ]],
      [[                =====================-       ]],
      [[               -====================-        ]],
      [[              :====================-         ]],
      [[             .====================-          ]],
      [[             ====================-           ]],
      [[            -===================-            ]],
      [[           :===================-             ]],
      [[          .------==============--------      ]],
      [[                -====================-       ]],
      [[               .====================-        ]],
      [[               ====================-         ]],
      [[              :===================:          ]],
      [[             .===================.           ]],
      [[             -==================.            ]],
      [[            :==================:....         ]],
      [[            ======================:          ]],
      [[            ....-===============:            ]],
      [[                ==============-.             ]],
      [[               .=============.               ]],
      [[               -===========:                 ]],
      [[              .==========-                   ]],
      [[              -========-.                    ]],
      [[              ========:                      ]],
      [[             :======-                        ]],
      [[             -====-.                         ]],
      [[            .====:                           ]],
      [[            -==:                             ]],
      [[            =-                               ]],
      [[           :.                                ]]
    }

    dashboard.section.header.opts.hl = 'Text'

    dashboard.section.buttons.val = {
      -- button('SPC e', '  File Explorer'),
      button('SPC f', '  Find File'),
      button('SPC g', '  Find Word'),
      button('SPC n', '  New File')
    }

    dashboard.section.footer.val = footer()

    dashboard.section.footer.opts.hl = 'Constant'

    alpha.setup(dashboard.opts)
  end
}
