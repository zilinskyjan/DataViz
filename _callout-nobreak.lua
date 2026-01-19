-- Lua filter to prevent callout boxes from breaking across pages in PDF
-- This only affects PDF output, not HTML

function Div(div)
  -- Check if this is a callout div
  if div.classes then
    for _, class in ipairs(div.classes) do
      if string.match(class, "^callout") then
        -- Add LaTeX commands to prevent page breaks
        -- This will be inserted before the callout in PDF output
        local needspace = pandoc.RawBlock('latex', '\\needspace{5\\baselineskip}\\nopagebreak[4]')
        return {needspace, div}
      end
    end
  end
  return nil
end
