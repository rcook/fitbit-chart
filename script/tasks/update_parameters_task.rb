require_relative '../lib/ssm'
require_relative 'task'

class UpdateParametersTask < Task
  def run(manifest, repo_dir)
    trace 'UpdateParametersTask' do
      values = Temp.with_temp_file do |output_path|
        command = manifest.ssm_parameters_command % {
          repo_dir: repo_dir,
          output_path: output_path
        }
        Shell.check_run command
        YAML.load(File.read(output_path))
      end
      values.each do |k, v|
        SSM.put_parameter k, v, :secure_string
      end
    end
  end

  private

  def get_parameter_value(p)
    return p['value'] if p.include?('value')

    if p.include?('command')
      command = p['command']
      output = Shell.check_run(command).chomp
      puts "output=#{output.inspect}"
      exit 1
    end

    raise 'Not implemented'
  end
end
