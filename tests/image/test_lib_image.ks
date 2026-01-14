use std::IO
use std::lib::Base64
use std::lib::Image

IO = std::IO
Base64 = std::lib::Base64
Image = std::lib::Image

img_path = "tmp_image_test.png"
if IO.exists(img_path)
  IO.remove(img_path)
end

rgba = [
  255, 0, 0, 255,
  0, 255, 0, 255
]
encoded = Base64.encode_bytes(rgba)
Image.save_png(img_path, 2, 1, encoded)

img = Image.load_png(img_path)
puts img.width
puts img.height
puts img.rgba == encoded

if IO.exists(img_path)
  IO.remove(img_path)
end
