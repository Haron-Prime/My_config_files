-- sopcast hook for mpv
--
-- How to use this script:
-- 1. Move this script to ~/.config/mpv/scripts
-- 2. Make sure sp-sc-auth/sp-sc is in your $PATH or in ~/.config/mpv
-- 3. Install luasocket

local utils = require 'mp.utils'
local msg = require 'mp.msg'
local socket = require 'socket'

local vars = {
    sopcast = "sp-sc-auth",
    sopcast_names = {"sp-sc-auth", "sp-sc"},
    sopcast_args = "",
    sock = nil,
    url = "",
    port = 8902,
    timeout = 10,
    debug = false
}

local function sleep(s)
    local ntime = os.time() + s
    repeat until os.time() > ntime
end

local function exec(args)
    local ret = utils.subprocess({args = args})
    return ret.status, ret.stdout
end

local function find_unused_port()
    local command = { "ps", "-o", "command" }
    local status, processes = exec(command)

    local result = vars.port
    for i in string.gmatch(processes, "[^\r\n]+") do
        for j, name in ipairs(vars.sopcast_names) do
            if not (string.find(i, name .. " sop://", 1, true) == nil) then
                local port = tonumber(i:sub(i:match(".* ()")))
                if (port >= result) then
                    result = port+1
                end
            end
        end
    end
    return result
end

local function get_info()
    vars.sock:send("s\n")
    local response, status, partial = vars.sock:receive()
    info = {}
    for ele in response:gmatch("%w+") do table.insert(info, ele) end
    return info
end

function debug_on_tick(event)
    if vars.sock == nil then return end
    local info = get_info()
    msg.warn("cache: " .. info[1] .. " ur: " .. info[2] .. " dr: " .. info[3] ..
             " us: " .. info[4] .. " ds: " .. info[5] .. " peers: " .. info[6])
end

local function on_start()
    vars.url = mp.get_property("stream-open-filename")

    if (vars.url:find("sop://") == 1) then
        -- find sopcast binary, search various names
        for i, name in ipairs(vars.sopcast_names) do
            local sopcast_bin = mp.find_config_file(name)
            if not (sopcast_bin == nil) then
                msg.verbose("found sopcast at: " .. sopcast_bin)
                vars.sopcast = sopcast_bin
                break
            end
        end

        -- find an unused port, needed for simultaneous streams
        vars.port = find_unused_port()

        -- start sopcast
        msg.verbose("starting sopcast on port " .. vars.port)
        vars.sopcast_args = vars.url .. " 3908 " .. vars.port
        io.popen(vars.sopcast .. " " .. vars.sopcast_args .. " &")

        -- try up to vars.timeout times to connect to sopcast
        local timeout = 0
        while (vars.sock == nil and timeout < vars.timeout) do
            timeout = timeout + 1
            vars.sock = socket.connect("localhost", vars.port)
            sleep(1)
        end

        -- check if connection worked
        if vars.sock ~= nil then
            -- print debug info on tick
            if vars.debug then
                -- prepare sopcast for cache status
                vars.sock:settimeout(10)
                assert(vars.sock:send("state\ns\n"))
                mp.register_event("tick", debug_on_tick)
            end

            -- open the local sopcast stream
            mp.set_property("stream-open-filename", "http://localhost:" .. vars.port)
        else
            msg.warn("could not connect to sopcast. unresponsive or unuvailable channel.")
            mp.command("quit")
            return
        end

    end
end

function on_end(event)
    if vars.sock ~= nil then
        mp.unregister_event(tick)
        vars.sock:close()
        os.execute("pkill -f \"".. vars.sopcast .. " " .. vars.sopcast_args .. "\"" )
        msg.verbose("sopcast terminated.")
    end
end

mp.add_hook("on_load", 50, on_start)
mp.add_hook("on_unload", 50, on_end)
