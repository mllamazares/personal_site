
require 'image_optim'
require 'image_optim/pack'
require 'digest'
require 'fileutils'

Jekyll::Hooks.register :site, :post_write do |site|
  puts "    [ImageOptimizer] Checking for images to optimize..."

  # Setup Cache Directory
  cache_base = File.join(site.source, ".jekyll-cache", "image_optim_assets")
  FileUtils.mkdir_p(cache_base)

  # Initialize ImageOptim
  # Threads are internal to image_optim workers, but we can also parallelize the loop.
  image_optim = ImageOptim.new(
    :nice => 0,
    :threads => true,
    :pack => true,
    :skip_missing_workers => true,
    :jpegoptim => { :allow_lossy => true, :max_quality => 85, :strip => :all },
    :optipng => { :level => 2 },
    :svgo => false
  )

  site_dest = site.dest
  # glob all images
  images = Dir.glob(File.join(site_dest, "**", "*.{jpg,jpeg,png,gif,webp}"))

  # We will process in parallel using threads
  queue = Queue.new
  images.each { |img| queue << img }
  
  # Stats
  processed_count = 0
  cached_count = 0
  total_saved = 0
  stats_mutex = Mutex.new

  # Worker Threads
  workers = [8, (images.size / 2.0).ceil].min
  workers = 1 if workers < 1

  puts "    [ImageOptimizer] Processing #{images.size} images with #{workers} threads..."

  threads = Array.new(workers) do
    Thread.new do
      until queue.empty?
        begin
          path = queue.pop(true)
        rescue ThreadError
          break
        end

        next unless File.file?(path)

        # 1. Compute Hash of the current file in _site (which is fresh from source)
        content = File.read(path)
        digest = Digest::SHA1.hexdigest(content)
        ext = File.extname(path)
        
        # 2. Check locally cached optimized version
        cached_file = File.join(cache_base, "#{digest}#{ext}")

        if File.exist?(cached_file)
          # CACHE HIT: Copy cached file to _site, overwriting the unoptimized one
          FileUtils.cp(cached_file, path)
          stats_mutex.synchronize { cached_count += 1 }
        else
          # CACHE MISS: Optimize in place, then save to cache
          original_size = File.size(path)
          
          # Optimize (this modifies 'path' in place)
          image_optim.optimize_image!(path)
          
          # Save result to cache
          FileUtils.cp(path, cached_file)
          
          new_size = File.size(path)
          diff = original_size - new_size
          
          stats_mutex.synchronize do 
            processed_count += 1
            total_saved += diff
          end
        end
      end
    end
  end

  threads.each(&:join)

  puts "    [ImageOptimizer] Done. Optimized: #{processed_count}, Cached: #{cached_count}, Saved: #{total_saved} bytes."
end
