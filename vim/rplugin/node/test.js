var fmt = require('util').format,
    numCalls = 0

function incrementCalls() {
    if ( numCalls == 5 ) {
        throw new Error('Too many calls!')
    }
    numCalls++
}

plugin.commandSync('Cmd', {
    range: '',
    nargs: '*',
}, function( nvim, args, range, cb ) {
    try {
        incrementCalls()
        nvim.setCurrentLine(
            fmt('Command: Called', numCalls, 'times, args:', args, 'range:', range),
            cb )
    } catch ( err ) {
        cb( err )
    }
})

plugin.autocmdSync('BufEnter', {
    pattern: '*.js',
    eval: 'expand("<afile>")'
}, function( nvim, filename, cb ) {
    try {
        incrementCalls()
        nvim.setCurrentLine(
            fmt('Autocmd: Called', numCalls, 'times, file:', filename), cb )
    } catch ( err ) {
        cb( err )
    }
})

plugin.function('Func', function( nvim, args ) {
    try {
        incrementCalls()
        nvim.setCurrentLine( fmt('Function: Called', numCalls, 'times, args:', args) )
    } catch ( err ) {}
})
