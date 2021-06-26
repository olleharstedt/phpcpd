<?php declare(strict_types=1);
/*
 * This file is part of PHP Copy/Paste Detector (PHPCPD).
 *
 * (c) Sebastian Bergmann <sebastian@phpunit.de>
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */
namespace SebastianBergmann\PHPCPD\Detector\Strategy;

use function array_keys;
use function file_get_contents;
use function is_array;
use function token_get_all;
use Exception;
use SebastianBergmann\PHPCPD\CodeClone;
use SebastianBergmann\PHPCPD\CodeCloneFile;
use SebastianBergmann\PHPCPD\CodeCloneMap;
use SebastianBergmann\PHPCPD\Detector\Strategy\SuffixTree\AbstractToken;
use SebastianBergmann\PHPCPD\Detector\Strategy\SuffixTree\ApproximateCloneDetectingSuffixTree;
use SebastianBergmann\PHPCPD\Detector\Strategy\SuffixTree\Sentinel;
use SebastianBergmann\PHPCPD\Detector\Strategy\SuffixTree\Token;

/**
 * For the design of the algorithm, all credits go to the authors of "Do Code Clones Matter?".
 */
final class SuffixTreeStrategy extends AbstractStrategy
{
    /**
     * @var AbstractToken[]
     */
    private $word = [];

    /**
     * @var ?CodeCloneMap
     */
    private $result;

    /**
     * @var CloneConsumer
     */
    private $consumer;

    public function processFile(string $file, CodeCloneMap $result): void
    {
        $content = file_get_contents($file);
        $tokens  = token_get_all($content);

        foreach (array_keys($tokens) as $key) {
            $token = $tokens[$key];

            if (is_array($token)) {
                if (!isset($this->tokensIgnoreList[$token[0]])) {
                    $this->word[] = new Token(
                        $token[0],
                        token_name($token[0]),
                        $token[2],
                        $file,
                        $token[1]
                    );
                }
            }
        }

        $this->result = $result;
    }

    public function postProcess(): void
    {
        if (empty($this->result)) {
            throw new Exception('Missing result');
        }

        assert_options(ASSERT_ACTIVE, 1);
        assert_options(ASSERT_WARNING, 1);

        require_once __DIR__ . '/SuffixTree/tmp.php';
        $consumer = new SuffixTree\GapDetectingCloneConsumer($this->word);
        //$consumer = new SuffixTree\CloneConsumer($this->word);

        $tree       = new ApproximateCloneDetectingSuffixTree($this->word);
        $cloneInfos = $tree->findClones(
            $this->config->getMinTokens(),
            $this->config->getEditDistance(),
            $this->config->getHeadEquality(),
            $consumer
        );

        /*
        foreach ($cloneInfos as $cloneInfo) {
            $others = $cloneInfo->otherClones->extractFirstList();

            for ($j = 0; $j < count($others); $j++) {
                $otherStart = $others[$j];
                $t          = $this->word[$otherStart];
                $lastToken  = $this->word[$cloneInfo->position + $cloneInfo->length];
                // If we stumbled upon the Sentinel, rewind one step.
                if ($lastToken instanceof Sentinel) {
                    $lastToken = $this->word[$cloneInfo->position + $cloneInfo->length - 2];
                }
                $lines = $lastToken->line - $cloneInfo->token->line;
                $this->result->add(
                    new CodeClone(
                        new CodeCloneFile($cloneInfo->token->file, $cloneInfo->token->line),
                        new CodeCloneFile($t->file, $t->line),
                        $lines,
                        // TODO: Double check this
                        $otherStart + 1 - $cloneInfo->position
                    )
                );
            }
        }
         */
        $classes = $consumer->getCloneClasses();
        foreach ($classes as $class) {
            var_dump($class);
        }
    }

    public function setConsumer(CloneConsumer $consumer): void
    {
        $this->consumer = $consumer;
    }
}
