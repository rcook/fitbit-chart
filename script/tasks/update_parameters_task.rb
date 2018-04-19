require_relative '../lib/ssm'
require_relative 'task'

class UpdateParametersTask < Task
  def run(manifest, repo_dir)
    trace 'UpdateParametersTask' do
      ssm_parameters = manifest.ssm_parameters
      ssm_parameters.each do |section|
        private_store_path = File.expand_path(section.private_store_file, manifest.dir)
        values = YAML.load(File.read(private_store_path))
        section.parameters.each do |p|
          #SSM.put_parameter p, values.fetch(p), :secure_string
          puts get_parameter_value(values.fetch(p))
        end
      end
    end
  end

  private

  def get_parameter_value(p)
    return p['value'] if p.include?('value')

    if p.include?('command')
      command = p['command']
      output = Shell.check_capture(command).chomp
      puts "output=#{output.inspect}"
      exit 1
    end

    raise 'Not implemented'
  end
end
