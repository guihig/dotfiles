" let python_path = system('echo (pipenv --py)')
" let g:python3_host_prog = python_path

function! ConnectToPipenvKernel()
  let l:kernel = system('echo "ipykernel_"(basename (pwd)) | tr -d "\n"')
  call IPyConnect('--kernel', l:kernel, '--no-window')
endfunction

command! -nargs=0 ConnectToPipenvKernel call ConnectToPipenvKernel()
command! -nargs=0 RunQtConsole 
      \call jobstart("jupyter qtconsole --existing --JupyterWidget.include_other_output=True")
