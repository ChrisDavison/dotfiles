import time
import pyperclip
import sys
import os
import subprocess
sys.path.append(os.path.dirname(__file__))
from dla import main as nas_download_add
from download import list_downloads
from download import main as nas_download


def exit_msg():
    # print("Complete.")
    print("Complete. (NO SLEEP)")
    # time.sleep(1)
    os.exit(0)


def nas_jump_submenu():
    os.system('cls')
    menu = [
        "1: Refile",
        "2: ASMR",
        "3: eBooks"
    ]
    print("\n".join(menu))
    response = input("\n> ")
    print(response)
    os.system('cls')
    if response == '1':
        subprocess.run(["explorer.exe", "Y:\\refile"], capture_output=True)
    elif response == '2':
        subprocess.run(["explorer.exe", "Y:\\videos\\asmr"], capture_output=True)
    elif response == '3':
        subprocess.run(["explorer.exe", "Y:\\ebooks"], capture_output=True)
    else:
        return


def main():
    menu = [
        "1: NAS save clipboard to download list",
        "2: NAS quick save clipboard to download list",
        "3: NAS list downloads",
        "4: View NAS (SUBMENU)",
        "q: QUIT"
    ]
    print("\n".join(menu))
    response = input("\n> ")
    print(response)
    os.system('cls')
    if response == 'q':
        os.exit(0)
    elif response == '1':
        nas_download_add([], False)
    elif response == '2':
        nas_download_add([], True)
    elif response == '3':
        list_downloads()
        input('Press ENTER to exit')
    elif response == '4':
        nas_jump_submenu()
    else:
        print("UNRECOGNISED")
    exit_msg()


if __name__ == "__main__":
    main()
