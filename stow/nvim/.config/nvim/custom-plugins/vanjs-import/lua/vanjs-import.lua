local M = {}

function M.add_vanjs_import()
  -- Get the symbol under the cursor
  local symbol = vim.fn.expand("<cword>")

  if symbol == "" then
    vim.notify("No symbol under cursor", vim.log.levels.WARN)
    return
  end

  -- Search for the van.tags import line
  local bufnr = vim.api.nvim_get_current_buf()
  local lines = vim.api.nvim_buf_get_lines(bufnr, 0, -1, false)

  local import_line_num = nil
  local import_line = ""

  for i, line in ipairs(lines) do
    if line:match("van%.tags") and line:match("const%s*{") then
      import_line_num = i
      import_line = line
      break
    end
  end

  if not import_line_num then
    vim.notify("Could not find van.tags import line", vim.log.levels.WARN)
    return
  end

  -- Check if symbol is already imported
  local imports = import_line:match("const%s*{%s*([^}]*)%s*}")
  if not imports then
    vim.notify("Could not parse import line", vim.log.levels.ERROR)
    return
  end

  -- Check if the symbol already exists
  for imported in imports:gmatch("[%w_]+") do
    if imported == symbol then
      vim.notify(symbol .. " is already imported", vim.log.levels.INFO)
      return
    end
  end

  -- Add the new symbol to the import list
  local new_imports = imports:gsub("%s+$", "") .. ", " .. symbol
  local new_line = import_line:gsub("const%s*{%s*[^}]*%s*}", "const { " .. new_imports .. " }")

  -- Update the buffer
  vim.api.nvim_buf_set_lines(bufnr, import_line_num - 1, import_line_num, false, { new_line })

  vim.notify("Added " .. symbol .. " to van.tags import", vim.log.levels.INFO)
end

return M
