local M = {}

---The file system path separator for the current platform.
M.path_separator = "/"
M.is_windows = vim.fn.has("win32") == 1 or vim.fn.has("win32unix") == 1
if M.is_windows == true then
	M.path_separator = "\\"
end

-- See :help base-directories for more
M.config = vim.fn.stdpath("config") -- E.g., ~/.config/bvim/
M.data = vim.fn.stdpath("data")     -- E.g., ~/.local/share/bvim/
M.state = vim.fn.stdpath("state")   -- E.g., ~/.local/state/bvim/
M.cache = vim.fn.stdpath("cache")   -- E.g., ~/.cache/bvim

---Split string into a table of strings using a separator.
---@param inputString string The string to split.
---@param sep string The separator to use.
---@return table table A table of strings.
M.split = function(inputString, sep)
	if sep == nil then
		error("Two arguments expected")
	end

	local fields = {}

	local pattern = string.format("([^%s]+)", sep)
	local _ = string.gsub(inputString, pattern, function(c)
		fields[#fields + 1] = c
	end)

	return fields
end

---Joins arbitrary number of paths together.
---@param ... string The paths to join.
---@return string
M.path_join = function(...)
	local args = { ... }
	if #args == 0 then
		return ""
	end

	local all_parts = {}
	if type(args[1]) == "string" and args[1]:sub(1, 1) == M.path_separator then
		all_parts[1] = ""
	end

	for _, arg in ipairs(args) do
		local arg_parts = M.split(arg, M.path_separator)
		vim.list_extend(all_parts, arg_parts)
	end
	return table.concat(all_parts, M.path_separator)
end

---@param name string
---@return boolean?
M.file_exists = function(name)
	local f = io.open(name, "r")
	return f ~= nil and io.close(f)
end

function M.mkdir_p(path)
  local parts = M.split(path, M.path_separator)
  local parts_cumulative = {}

  if vim.fn.has("unix") then
    table.insert(parts_cumulative, "/")
  end

  for _, part in ipairs(parts) do
    table.insert(parts_cumulative, part)
    table.insert(parts_cumulative, "/")

    local path_cumulative = M.path_join(unpack(parts_cumulative))

    if not M.file_exists(path_cumulative) then
      local completed = vim.system({ "mkdir", path_cumulative }):wait()
      if completed.code ~= 0 then
        error("Failed to create directory: " .. path_cumulative)
      end
    end
  end
end

---@param path string
function M.dir_empty(path)
  local file_names = vim.system({ "ls", path }):wait()
  return file_names.stdout == ""
end

---@param t1 any[]
---@param t2 any[]
function M.subtract_tables(t1, t2)
  local lookup = {}
  for _, v in ipairs(t2) do
    lookup[v] = true
  end

  for i = #t1, 1, -1 do
    if lookup[t1[i]] then
      table.remove(t1, i)
    end
  end
  return t1
end

return M
