% Generate some points around a line
intercept = -10; slope = 3;
npts = 50; noise = 80;
xs = 10 + rand(npts, 1) * 90;
ys = slope * xs + intercept + rand(npts, 1) * noise;

% Plot the randomly generated points
figure; plot(xs, ys, 'b.', 'MarkerSize', 5)

% Fit these points to a line
A = [xs, ys, -1 * ones(npts, 1)];
[U, S, V] = svd(A);
fit = V(:, end - 1);

% Get the coefficients a, b, c in ax + by + c = 0
a = fit(1); b = fit(2); c = fit(3);

% Compute slope m and intercept i for y = mx + i
slope_est = -a / b;
intercept_est = c / b;

% Plot fitted line on top of old data
ys_est = slope_est * xs + intercept_est;
figure; plot(xs, ys, 'b.', 'MarkerSize', 5);
hold on; plot(xs, ys_est, 'r-')
