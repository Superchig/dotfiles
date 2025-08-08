-- Set status line
---@return string
function StatuslineMode()
	local mode_info = vim.api.nvim_get_mode()
	local current_mode = mode_info.mode
	if current_mode == "n" then
		return "Normal"
	elseif current_mode == "i" then
		return "Insert"
	elseif current_mode == "v" then
		return "Visual"
	elseif current_mode == "V" then
		return "Visual Line"
	elseif current_mode == "\22" then -- <C-V>
		return "Visual Block"
	elseif current_mode == "R" then
		return "Replace"
	elseif current_mode == "s" then
		return "Select"
	elseif current_mode == "S" then
		return "Select Line"
	elseif current_mode == "\19" then -- <C-S>
		return "Select Block"
	elseif current_mode == "c" then
		return "Command"
	else
		return "Unknown Mode: " .. current_mode
	end
end

---@return string
function StatuslineFiletype()
	local ft = vim.bo.filetype
	if ft == "" then
		return ""
	end

	return " | " .. "[" .. ft .. "]"
end

---@return string
function StatuslineDiagnostics()
	local diagnostics = vim.diagnostic.get(0, {})
	local severity_counts = {
		[vim.diagnostic.severity.ERROR] = 0,
		[vim.diagnostic.severity.WARN] = 0,
		[vim.diagnostic.severity.INFO] = 0,
		[vim.diagnostic.severity.HINT] = 0,
	}
	for _, diagnostic in ipairs(diagnostics) do
		severity_counts[diagnostic.severity] = severity_counts[diagnostic.severity] + 1
	end

	local parts = {}
	if severity_counts[vim.diagnostic.severity.ERROR] > 0 then
		table.insert(parts, "E " .. severity_counts[vim.diagnostic.severity.ERROR])
	end
	if severity_counts[vim.diagnostic.severity.WARN] > 0 then
		table.insert(parts, "W " .. severity_counts[vim.diagnostic.severity.WARN])
	end
	if severity_counts[vim.diagnostic.severity.INFO] > 0 then
		table.insert(parts, "I " .. severity_counts[vim.diagnostic.severity.INFO])
	end
	if severity_counts[vim.diagnostic.severity.HINT] > 0 then
		table.insert(parts, "H " .. severity_counts[vim.diagnostic.severity.HINT])
	end
	if #parts > 0 then
		table.insert(parts, "| ")
		return table.concat(parts, " ")
	else
		return ""
	end
end

-- The default statusline looks like this: set statusline=%<%f\ %h%w%m%r%=%-14.(%l,%c%V%)\ %P
local statusline_parts = {
	" ",
	"%{v:lua.StatuslineMode()}",
	" | ",

	"%{v:lua.StatuslineDiagnostics()}",

	"%<", -- Truncate line at start
	"%f ", -- Path to file in buffer, as typed or relative to cwd
	"%h", -- Help buffer flag, text is "[help]"
	"%w", -- Preview window flag, text is "[Preview]"
	"%m", -- Modified flag, text is "[+]"; "[-]" if 'modifiable' is off
	"%r", -- Readonly flag, text is "[RO]".

	"%=", -- Separator for a new section

	"%-4.P", -- Percentage through file of displayed window.  This is like the percentage described for 'ruler'
	" ",
	"%(",
	"%l,", -- Line number
	"%c", -- Column number
	"%V", -- Virtual column number as -{num}.  Not displayed if equal to 'c'.
	"%)",
	"%{v:lua.StatuslineFiletype()}",
	" ",
}
return table.concat(statusline_parts, "")
