if executable('opam')
  let s:merlin=substitute(system('opam config var share'),'\n$','','g') . "/merlin/vim"
  execute "set rtp+=" . s:merlin
endif
