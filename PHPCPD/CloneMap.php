<?php
/**
 * phpcpd
 *
 * Copyright (c) 2009, Sebastian Bergmann <sb@sebastian-bergmann.de>.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 *   * Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *
 *   * Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in
 *     the documentation and/or other materials provided with the
 *     distribution.
 *
 *   * Neither the name of Sebastian Bergmann nor the names of his
 *     contributors may be used to endorse or promote products derived
 *     from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
 * ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 *
 * @package   phpcpd
 * @author    Sebastian Bergmann <sb@sebastian-bergmann.de>
 * @copyright 2009 Sebastian Bergmann <sb@sebastian-bergmann.de>
 * @license   http://www.opensource.org/licenses/bsd-license.php  BSD License
 * @since     File available since Release 1.1.0
 */

/**
 * A map of exact clones.
 *
 * @author    Sebastian Bergmann <sb@sebastian-bergmann.de>
 * @copyright 2009 Sebastian Bergmann <sb@sebastian-bergmann.de>
 * @license   http://www.opensource.org/licenses/bsd-license.php  BSD License
 * @version   Release: @package_version@
 * @link      http://github.com/sebastianbergmann/phpcpd/tree
 * @since     Class available since Release 1.1.0
 */
class PHPCPD_CloneMap implements Countable, Iterator
{
    protected $clones = array();
    protected $clonesByFile = array();
    protected $position = 0;

    /**
     * Adds a clone to the map.
     *
     * @param PHPCPD_Clone $clone
     */
    public function addClone(PHPCPD_Clone $clone)
    {
        $this->clones[] = $clone;

        if (isset($this->clonesByFile[$clone->aFile])) {
            $this->clonesByFile[$clone->aFile][] = $clone;
        } else {
            $this->clonesByFile[$clone->aFile][] = array($clone);
        }

        if (isset($this->clonesByFile[$clone->bFile])) {
            $this->clonesByFile[$clone->bFile][] = $clone;
        } else {
            $this->clonesByFile[$clone->bFile][] = array($clone);
        }
    }

    /**
     * Returns the clones stored in this map.
     *
     * @return array
     */
    public function getClones()
    {
        return $this->clones;
    }

    /**
     * Returns the files the clones stored in this map occur in.
     *
     * @return array
     */
    public function getFilesWithClones()
    {
        return array_keys($this->clonesByFile);
    }

    /**
     * Returns the number of clones stored in this map.
     */
    public function count() {
        return count($this->clones);
    }

    /**
     * Rewinds the Iterator to the first element.
     */
    public function rewind() {
        $this->position = 0;
    }

    /**
     * Checks if there is a current element after calls to rewind() or next().
     *
     * @return boolean
     */
    public function valid() {
        return $this->position < count($this->clones);
    }

    /**
     * Returns the key of the current element.
     *
     * @return integer
     */
    public function key() {
        return $this->position;
    }

    /**
     * Returns the current element.
     *
     * @return PHPCPD_Clone
     */
    public function current() {
        return $this->clones[$this->position];
    }

    /**
     * Moves forward to next element.
     */
    public function next() {
        $this->position++;
    }
}
?>