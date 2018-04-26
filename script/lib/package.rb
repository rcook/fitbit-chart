require 'fileutils'
require 'set'
require_relative 'shell'
require_relative 'tracer'

class PackageBuildTask < Tracer
  def initialize
    super verbose: true
  end

  def run(repo_dir, target_name, excluded_dependencies, extra_files, package_path)
    trace 'Rebuilding code (native)' do
      Dir.chdir(repo_dir) do
        Shell.check_run('stack', 'build', '--no-docker')
      end
    end

    trace 'Rebuilding code (Docker)' do
      Dir.chdir(repo_dir) do
        Shell.check_run('stack', 'build', '--docker')
      end
    end

    local_install_root = trace 'Determining Stack local install root' do
      Dir.chdir(repo_dir) do
        Shell.check_capture('stack', 'path', '--docker', '--local-install-root').chomp
      end
    end
    target_dir = File.expand_path('bin', local_install_root)
    target_path = File.expand_path(target_name, target_dir)
    excluded_dependencies = Set.new(excluded_dependencies)

    all_dependencies = trace 'Getting binary dependencies' do
      get_all_dependencies(target_path)
    end

    dependencies = all_dependencies.reject { |f, _| excluded_dependencies.include?(f) }

    work_dir = File.expand_path('.package-work', repo_dir)
    input_dir = File.expand_path('input', work_dir)
    output_dir = File.expand_path('output', work_dir)

    FileUtils.remove_dir work_dir if Dir.exists?(work_dir)
    FileUtils.mkpath work_dir
    FileUtils.mkpath input_dir
    FileUtils.mkpath output_dir

    trace 'Copying dependencies' do
      dependencies.each do |f, p|
        dep_path = File.expand_path(f, target_dir)
        Shell.check_capture('stack', 'exec', '--docker', '--', 'cp', p, dep_path)
        FileUtils.cp dep_path, input_dir
      end
    end

    FileUtils.cp target_path, input_dir

    values = {
      target_name: target_name
    }

    trace 'Creating extra files' do
      extra_files.each do |p|
        source_text = File.read(p)
        output_text = source_text % values
        output_path = File.expand_path(File.basename(p), input_dir)
        File.write(output_path, output_text)
      end
    end

    FileUtils.chmod_R 0777, input_dir

    trace 'Creating zip archive' do
      Shell.check_capture("zip -j #{package_path} #{input_dir}/*")
    end
  end

  def get_all_dependencies(path)
    Shell.check_capture('stack', 'exec', '--docker', '--', 'ldd', path)
      .split(/\n+/)
      .map(&:strip)
      .map(&:split)
      .select { |l| l.size == 4 && l[1] == '=>' }
      .map { |l| [l[0], l[2]] }
  end
end

module Package
  def self.build(repo_dir, target_name, excluded_dependencies, extra_files)
    Temp.with_temp_file('.zip') do |package_path|
      PackageBuildTask.new.run repo_dir, target_name, excluded_dependencies, extra_files, package_path
      package_path
    end
  end
end
