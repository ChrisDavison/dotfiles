function reporevision
	[ -e ~/.reporevisions ]; and rm ~/.reporevisions; and touch ~/.reporevisions
for f in ~/devel/*
pushd $f
echo "$f (git rev-parse HEAD)" >> ~/.reporevisions
popd
end
end
