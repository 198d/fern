from setuptools import setup


setup(name='fern',
      description='Fern is a remote execution framework written in Hy',
      version='0.0.1',
      author='John Mackenzie',
      author_email='john@nineteeneightd.com',
      install_requires=['hy>=0.9.12',
                        'paramiko>=1.12'])
