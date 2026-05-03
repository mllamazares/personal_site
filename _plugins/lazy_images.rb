Jekyll::Hooks.register [:pages, :posts, :documents], :post_render do |item|
  next unless item.output_ext == '.html' && item.output

  item.output = item.output.gsub(/<img(?![^>]*\bloading=)([^>]*)>/i) do
    "<img loading=\"lazy\"#{$1}>"
  end
end
