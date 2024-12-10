local process = { _version = "0.0.1" }
ao = require('.ao')

function process.handle(msg, env)

    return ao.result({ Output = { data = msg.Data, print = true } })
end

return process