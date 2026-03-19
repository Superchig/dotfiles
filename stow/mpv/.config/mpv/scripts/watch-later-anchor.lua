local utils = require("mp.utils")
local options = require("mp.options")

local opts = {
	anchor_dir = "Ventoy", -- directory name to look for (required)
	history_subdir = "mpv/watch_later", -- history location relative to anchor
}

options.read_options(opts, "watch-later-anchor")

local function find_anchor(path)
	if opts.anchor_dir == "" then
		mp.msg.warn("watch-later-anchor: anchor_dir is not set, doing nothing.")
		return nil
	end

	local found = nil
	local current = path

	while true do
		local parent, recent = utils.split_path(current)
		local last_char = parent:sub(#parent, #parent)
		if last_char == "/" then
			parent = parent:sub(1, #parent - 1)
		end

		if parent == current or parent == "" then
			break
		end

		if recent == opts.anchor_dir then
			found = current
			break
		end

		current = parent
	end

	return found
end

local function md5sum(str)
	-- Write string to a temp file, then compute md5sum
	local tmpfile = os.tmpname()
	local f = io.open(tmpfile, "wb")
	if not f then
		error("Failed to open tmpfile")
	end

	f:write(str)
	f:close()

	local handle = io.popen("md5sum " .. tmpfile)
	if not handle then
		error("Failed to run md5sum on tmpfile")
	end

	local result = handle:read("*a")
	handle:close()
	os.remove(tmpfile)

	-- md5sum output format: "<hash>  <filename>"
	return result:match("^(%x+)")
end

---@param path string
---@return string
local function read_file(path)
	local file = io.open(path, "r") -- Open file in read mode
	if not file then
		error("Could not open file: " .. path)
	end
	local content = file:read("*a") -- "*a" reads the entire file
	file:close()
	return content
end

local function lines(s)
	if s:sub(-1) ~= "\n" then
		s = s .. "\n"
	end
	return s:gmatch("(.-)\n")
end

local function on_load()
	local path = mp.get_property("path")
	if not path then
		return
	end

	if not path:match("^/") then
		path = utils.join_path(utils.getcwd(), path)
	end

	local anchor = find_anchor(path)
	if not anchor then
		return
	end

	local watch_later_dir = utils.join_path(anchor, opts.history_subdir)
	os.execute(string.format('mkdir -p "%s"', watch_later_dir))
	mp.set_property("watch-later-dir", watch_later_dir)
	mp.msg.info("watch-later dir set to: " .. watch_later_dir)

	local watch_history_path, _ = utils.split_path(watch_later_dir)
	watch_history_path = utils.join_path(watch_history_path, "watch_history.jsonl")
	mp.set_property("watch-history-path", watch_history_path)
	mp.msg.info("watch-history-path set to: " .. watch_history_path)

	-- Setting watch-later dir and watch-history-path here isn't early enough for
	-- mpv to actually use those values when resuming playback, so we'll have to
	-- manually reimplement the functionality (oh, boy!).

	mp.msg.info(
		"Note that any built-in 'Resuming playback...' messages are false positives, since we don't set the watch flags early enough in this script"
	)
	local hash = md5sum(path)
	local watch_later_file_path = utils.join_path(watch_later_dir, hash)
	local content = read_file(watch_later_file_path)

	for line in lines(content) do
		if #line <= 0 or line:sub(1, 1) == "#" then
			goto continue
		end

		local _, _, key, value = line:find("(.*)=(.*)")
		mp.msg.info("Resuming playback with " .. key .. "=" .. value)
		mp.set_property(key, value)

		::continue::
	end
end

mp.add_hook("on_load", 50, on_load)
