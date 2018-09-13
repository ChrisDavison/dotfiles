function new --argument project
	mkdir $project
cd $project
git init
touch README.md
echo "# $project" >> README.md
git a README.md
git c -m "Initial commit"
end
