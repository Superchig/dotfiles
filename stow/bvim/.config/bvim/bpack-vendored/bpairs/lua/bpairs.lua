-- A plugin that inserts and removes pairs for you

local M = {}

local function get_surrounding_chars()
	local pos = vim.api.nvim_win_get_cursor(0)
	local line = pos[1] - 1
	local col = pos[2]

	local l = vim.api.nvim_buf_get_lines(0, line, line + 1, false)[1]

	local left_ch = string.sub(l, col, col)
	local right_ch = string.sub(l, col + 1, col + 1)

	return { left_ch, right_ch }
end

local function insert_twin(ch)
	local pos = vim.api.nvim_win_get_cursor(0)
	local line = pos[1]
	local col = pos[2]

	local surrounding_ch = get_surrounding_chars()
	local right_ch = surrounding_ch[2]

	if right_ch ~= ch then
		vim.api.nvim_paste(ch .. ch, false, -1)
	end

	vim.api.nvim_win_set_cursor(0, { line, col + #ch })
end

local function insert_pair(opener, closer)
	if opener == closer then
		insert_twin(opener)
	end

	vim.api.nvim_paste(opener .. closer, false, -1)

	local pos = vim.api.nvim_win_get_cursor(0)
	local line = pos[1]
	local col = pos[2]

	vim.api.nvim_win_set_cursor(0, { line, col - 1 })
end

local function insert_closer(opener, closer)
	if opener == closer then
		insert_twin(opener)
		return
	end

	local pos = vim.api.nvim_win_get_cursor(0)
	local line = pos[1] - 1
	local col = pos[2]

	local surrounding_ch = get_surrounding_chars()

	local right_ch = surrounding_ch[2]

	if right_ch == closer then
		vim.api.nvim_win_set_cursor(0, { line + 1, col + 1 })
	else
		vim.api.nvim_paste(closer, false, -1)
	end
end

local function delete_closer(pairs)
	for _, pair in ipairs(pairs) do
		local opener = pair[1]
		local closer = pair[2]

		local surrounding_ch = get_surrounding_chars()
		local left_ch = surrounding_ch[1]
		local right_ch = surrounding_ch[2]

		if left_ch == opener and right_ch == closer then
			return "<ESC>xa<BS>"
		end
	end

	return "<BS>"
end

---@param pairs string[][]
function M.setup(pairs)
	-- Insert closing bracket when typing opening bracket
	for _, pair in ipairs(pairs) do
		vim.keymap.set("i", pair[1], function()
			insert_pair(pair[1], pair[2])
		end)
	end

	-- Skip over closing bracket if typed
	for _, pair in ipairs(pairs) do
		vim.keymap.set("i", pair[2], function()
			insert_closer(pair[1], pair[2])
		end)
	end

	-- Remove closing bracket when deleting opening bracket
	vim.keymap.set("i", "<BS>", function()
		return delete_closer(pairs)
	end, { expr = true })
end

return M
