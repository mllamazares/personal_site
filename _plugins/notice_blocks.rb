# Transform GitHub/Obsidian-style alert blockquotes into <div class="notice notice-TYPE"> blocks.
# Source markdown:
#   > [!NOTE]
#   > Body text.
# Or with a custom title (Obsidian-style):
#   > [!NOTE] Custom Title
#   > Body text.
# If no custom title is supplied, the type name is used (capitalized).
Jekyll::Hooks.register [:pages, :posts, :documents], :post_render do |item|
  next unless item.output_ext == '.html' && item.output

  item.output = item.output.gsub(
    /<blockquote>\s*<p>\s*\[!(NOTE|TIP|IMPORTANT|WARNING|CAUTION)\]([^\n<]*)(?:<br\s*\/?>)?\s*(.*?)<\/blockquote>/m
  ) do
    type = Regexp.last_match(1).downcase
    title = Regexp.last_match(2).strip
    body = Regexp.last_match(3)
    label = title.empty? ? type : title
    %(<div class="notice notice-#{type}"><div class="notice-title">#{label}</div><p>#{body}</div>)
  end
end
