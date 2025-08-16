#!python3

import subprocess
import argparse
import sys
import os
import itertools
import requests
import json
from pathlib import Path

#---------------------------------------
#		CONFIG
#---------------------------------------

VERSION_BEGIN_STR = "set(BLT_VERSION "
VERSION_END_STR = ")"

#---------------------------------------
#	      DO NOT TOUCH
#---------------------------------------

USER_HOME = Path.home()
ENVIRONMENT_DATA_LOCATION = USER_HOME / ".brett_scripts.env"

if sys.platform.startswith("win"):
	CONFIG_FILE_DIRECTORY = Path(os.getenv('APPDATA') + "\blt")
	CONFIG_FILE_LOCATION = Path(CONFIG_FILE_DIRECTORY + "\commit_config.json")
else:
	XDG_CONFIG_HOME = os.environ.get('XDG_CONFIG_HOME')
	if XDG_CONFIG_HOME is None:
		XDG_CONFIG_HOME = USER_HOME / ".config"
	else:
		XDG_CONFIG_HOME = Path(XDG_CONFIG_HOME)
	
	if len(str(XDG_CONFIG_HOME)) == 0:
		XDG_CONFIG_HOME = USER_HOME
	CONFIG_FILE_DIRECTORY = XDG_CONFIG_HOME / "blt"
	CONFIG_FILE_LOCATION = CONFIG_FILE_DIRECTORY / "commit_config.json"

class Config:
	def __init__(self):
		# Inline with semantic versioning it doesn't make sense to branch / release on minor
		self.branch_on_major = True
		self.branch_on_minor = False
		self.release_on_major = True
		self.release_on_minor = True
		self.main_branch = "main"
		self.patch_limit = -1
	
	def toJSON(self):
		return json.dumps(self, default=lambda o: o.__dict__, sort_keys=True, indent=4)
	
	def fromJSON(file):
		with open(file, "r") as f:
			j = json.load(f)
			obj = Config()
			[setattr(obj, key, val) for key, val in j.items() if hasattr(obj, key)]
			if obj.branch_on_minor:
				obj.branch_on_major = True
			return obj

	def from_file(file):
		values = {}
		if (not os.path.exists(file)):
			return Config()

		with open(file, "r") as f:
			j = json.load(f)
			obj = Config()
			[setattr(obj, key, val) for key, val in j.items() if hasattr(obj, key)]
			return obj

	def save_to_file(self, file):
		dir_index = str(file).rfind("/")
		dir = str(file)[:dir_index]
		if not os.path.exists(dir):
			print(f"Creating config directory {dir}")
			os.makedirs(dir)
		with open(file, "w") as f:
			json.dump(self, f, default=lambda o: o.__dict__, sort_keys=True, indent=4)
			

class EnvData:
	def __init__(self, github_username = '', github_token = ''):
		self.github_token = github_token
		self.github_username = github_username
  
	def get_env_from_file(file):
		f = open(file, "rt")
		values = {}
		for line in f:
			if line.startswith("export"):
				content = line.split("=")
				for idx, c in enumerate(content):
					content[idx] = c.replace("export", "").strip()
				values[content[0]] = content[1].replace("\"", "").replace("'", "")
		try:
			github_token = values["github_token"]
		except Exception:
			print("Failed to parse github token!")
		try:
			github_username = values["github_username"]
		except:
			print("Failed to parse github username! Assuming you are me!")
			github_username = "Tri11Paragon"
		return EnvData(github_username=github_username, github_token=github_token)

def open_process(command, print_out = True):
	process = subprocess.Popen(command, stderr=subprocess.PIPE, stdout=subprocess.PIPE)
	stdout, stderr = process.communicate()
	exit_code = process.wait()
	str_out = stdout.decode('utf8')
	str_err = stderr.decode('utf8')
	if print_out and len(str_out) > 0:
		print(str_out, end='')
	if print_out and len(str_err) > 0:
		print(str_err, end='')
	#print(stdout, stderr, exit_code)
	return (stdout, stderr, exit_code)

def load_cmake():
	cmake_file = open("CMakeLists.txt", 'r')
	cmake_text = cmake_file.read()
	cmake_file.close()
	return cmake_text

def write_cmake(cmake_text):
	cmake_file = open("CMakeLists.txt", 'w')
	cmake_file.write(cmake_text)
	cmake_file.close()

def get_version(cmake_text):
	begin = cmake_text.find(VERSION_BEGIN_STR) + len(VERSION_BEGIN_STR)
	end = cmake_text.find(VERSION_END_STR, begin)
	return (cmake_text[begin:end], begin, end)

def split_version(cmake_text):
	version, begin, end = get_version(cmake_text)
	version_parts = version.split('.')
	return (version_parts, begin, end)

def recombine(cmake_text, version_parts, begin, end):
	constructed_version = version_parts[0] + '.' + version_parts[1] + '.' + version_parts[2]
	constructed_text_begin = cmake_text[0:begin]
	constructed_text_end = cmake_text[end::]
	return constructed_text_begin + constructed_version + constructed_text_end

def inc_major(cmake_text):
	version_parts, begin, end = split_version(cmake_text)
	version_parts[0] = str(int(version_parts[0]) + 1)
	version_parts[1] = '0'
	version_parts[2] = '0'
	return recombine(cmake_text, version_parts, begin, end)

def inc_minor(cmake_text):
	version_parts, begin, end = split_version(cmake_text)
	version_parts[1] = str(int(version_parts[1]) + 1)
	version_parts[2] = '0'
	return recombine(cmake_text, version_parts, begin, end)

def inc_patch(config: Config, cmake_text):
	version_parts, begin, end = split_version(cmake_text)
	if config.patch_limit > 0 and int(version_parts[2]) + 1 >= config.patch_limit:
		return inc_minor(cmake_text)
	version_parts[2] = str(int(version_parts[2]) + 1)
	return recombine(cmake_text, version_parts, begin, end)

def make_branch(config: Config, name):
	print(f"Making new branch {name}")
	subprocess.call(["git", "checkout", "-b", name])
	subprocess.call(["git", "merge", config.main_branch])
	subprocess.call(["git", "checkout", config.main_branch])
 
def sync_branch(config: Config, version_parts, args):
    if config.branch_on_major:
        # Branch will be created.
        if args.minor:
            return;

def make_release(env: EnvData, name):
	print(f"Making new release {name}")
	repos_v = open_process(["git", "remote", "-v"])[0].splitlines()
	urls = []
	for line in repos_v:
		origin = ''.join(itertools.takewhile(str.isalpha, line.decode('utf8')))
		urls.append(open_process(["git", "remote", "get-url", origin], False)[0].decode('utf8').replace("\n", "").replace(".git", "").replace("https://github.com/", "https://api.github.com/repos/") + "/releases")
	urls = set(urls)
	data = {
		'tag_name': name,
		'name': name,
		'body': "Automated Release '" + name + "'",
		'draft': False,
		'prerelease': False
	}
	headers = {
		'Authorization': f'Bearer {env.github_token}',
		'Accept': 'application/vnd.github+json',
		'X-GitHub-Api-Version': '2022-11-28'
	}
	for url in urls:
		if not "github" in url:
			continue
		response = requests.post(url, headers=headers, data=json.dumps(data))
		if response.status_code == 201:
			print('Release created successfully!')
			release_data = response.json()
			print(f"Release URL: {release_data['html_url']}")
		else:
			print(f"Failed to create release: {response.status_code}")
			print(response.json())


def main():
	parser = argparse.ArgumentParser(
		prog="Commit Helper", 
		description="Help you make pretty commits :3")
	
	parser.add_argument("action", nargs='?', default=None)
	parser.add_argument("-p", "--patch", action='store_true', default=False, required=False)
	parser.add_argument("-m", "--minor", action='store_true', default=False, required=False)
	parser.add_argument("-M", "--major", action='store_true', default=False, required=False)
	parser.add_argument('-e', "--env", help="environment file", required=False, default=None)
	parser.add_argument('-c', "--config", help="config file", required=False, default=None)
	parser.add_argument("--create-default-config", action="store_true", default=False, required=False)
	parser.add_argument("--no-release", action="store_true", default=False, required=False)
	parser.add_argument("--no-branch", action="store_true", default=False, required=False)
	
	args = parser.parse_args()
 
	if args.create_default_config:
		config = Config()
		config.save_to_file(args.config if args.config is not None else CONFIG_FILE_LOCATION)
		return
 
	if args.env is not None:
		env = EnvData.get_env_from_file(args.e)
	else:
		env = EnvData.get_env_from_file(ENVIRONMENT_DATA_LOCATION)
  
	if args.config is not None:
		config = Config.from_file(args.config)
	else:
		config = Config.from_file(CONFIG_FILE_LOCATION)
	
	cmake_text = load_cmake()
	cmake_version = get_version(cmake_text)[0]
	print(f"Current Version: {cmake_version}")
	
	if not (args.patch or args.minor or args.major):
		try:
			if args.action is not None:
				type = args.action
			else:
				type = input("What kind of commit is this ((M)ajor, (m)inor, (p)atch)? ")
			
			if type.startswith('M'):
				args.major = True
			elif type.startswith('m'):
				args.minor = True
			elif type.startswith('p') or type.startswith('P') or len(type) == 0:
				args.patch = True
		except KeyboardInterrupt:
			print("\nCancelling!")
			return
	
	if args.major:
		print("Selected major")
		write_cmake(inc_major(cmake_text))
	elif args.minor:
		print("Selected minor")
		write_cmake(inc_minor(cmake_text))
	elif args.patch:
		print("Selected patch")
		write_cmake(inc_patch(config, cmake_text))

	subprocess.call(["git", "add", "*"])
	subprocess.call(["git", "commit"])
 
	cmake_text = load_cmake()
	version_parts = split_version(cmake_text)[0]
	if args.major:
		if config.branch_on_major:
			if not args.no_branch:
				make_branch(config, "v" + str(version_parts[0]))
	
	if args.minor:
		if config.branch_on_minor:
			if not args.no_branch:
				make_branch(config, "v" + str(version_parts[0]) + "." + str(version_parts[1]))
		elif config.branch_on_major:
			subprocess.call(["git", "checkout", "v" + str(version_parts[0])])
			subprocess.call(["git", "rebase", config.main_branch])
			subprocess.call(["git", "checkout", config.main_branch])

	if args.patch:
		if config.branch_on_minor:
			subprocess.call(["git", "checkout", "v" + str(version_parts[0]) + "." + str(version_parts[1])])
			subprocess.call(["git", "rebase", config.main_branch])
			subprocess.call(["git", "checkout", config.main_branch])
		elif config.branch_on_major:
			subprocess.call(["git", "checkout", "v" + str(version_parts[0])])
			subprocess.call(["git", "rebase", config.main_branch])
			subprocess.call(["git", "checkout", config.main_branch])
  
	sync_branch(config=config, version_parts=version_parts, args=args)
		
	subprocess.call(["sh", "-c", "git remote | xargs -L1 git push --all"])
 
	if args.major:
		if not args.no_release and config.release_on_major:
			make_release(env, "v" + str(version_parts[0]))
	if args.minor:
		if not args.no_release and config.release_on_minor:
			make_release(env, "v" + str(version_parts[0]) + "." + str(version_parts[1]))
  
if __name__ == "__main__":
	main()
